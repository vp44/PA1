#We load the required files
subject_train <-as.matrix(read.table("./train/subject_train.txt", header = FALSE,colClasses="numeric"))
x_train <-as.matrix(read.table("./train/X_train.txt", header = FALSE,colClasses="numeric"))
y_train <-as.matrix(read.table("./train/Y_train.txt", header = FALSE,colClasses="numeric"))

subject_test <-as.matrix(read.table("./test/subject_test.txt", header = FALSE,colClasses="numeric"))
x_test <-as.matrix(read.table("./test/X_test.txt", header = FALSE,colClasses="numeric"))
y_test <-as.matrix(read.table("./test/Y_test.txt", header = FALSE,colClasses="numeric"))


#We create a data set
FullDataset = rbind(cbind(subject_train,y_train,x_train),cbind(subject_test,y_test,x_test))
FullDatasetDataFrame <- data.frame(FullDataset)
colnames(FullDatasetDataFrame)<-c("Subject_id","activity_id",as.character(seq(ncol(FullDataset)-2)))

###Select columns as to what the part 2 says
features <- as.matrix(read.table("features.txt", header = FALSE))

featuresdataframe <- data.frame(keys = as.numeric(features[,1]), values = features[,2])

meanstdcolumns <-featuresdataframe[grepl("mean|std",featuresdataframe[,2],perl=T),1]
meanstdcolumns <-sapply(meanstdcolumns,function(x) x+2)


FullDatasetMeanStd <- cbind(FullDatasetDataFrame[,c(1,2)],FullDatasetDataFrame[,meanstdcolumns])

#change labels for part 3 and 4
activities_labels <- as.matrix(read.table("activity_labels.txt",header = FALSE))

activities_labels_dataframe <-data.frame(keys = as.numeric(activities_labels[,1]),values = activities_labels[,2])

descriptiveactivities <- sapply(FullDataset[,2],
                                function(x)
                                  as.character(activities_labels_dataframe[activities_labels_dataframe$keys==x,2]))

FullDatasetMeanStd$activity_id <-descriptiveactivities

VarNames <- colnames(FullDatasetMeanStd)

var_labels <-as.matrix(read.table("features.txt",header = FALSE))
var_labels_dataframe <- data.frame(keys = as.numeric(var_labels[,1]),values = var_labels[,2])

descriptive_var <- sapply(VarNames,
                          
                          function(x) 
                            if(nrow(var_labels_dataframe[var_labels_dataframe$keys==as.numeric(x),])==1)
                            {
                              as.character(var_labels_dataframe[var_labels_dataframe$keys==x,2])
                            }
                            else
                            {
                              x
                            })
                              
descriptive_var <- gsub("mean\\(\\)"," Mean Value ", descriptive_var)
descriptive_var <- gsub("std\\(\\)"," standard deviation ", descriptive_var)
descriptive_var <- gsub("mean"," Mean Value ", descriptive_var)
descriptive_var <- gsub("std"," standard deviation ", descriptive_var)
descriptive_var <- gsub("^f"," Freq Domain ", descriptive_var)
descriptive_var <- gsub("^t"," Time Domain ", descriptive_var)

descriptive_var <- gsub("Acc"," Acceleration ", descriptive_var)
descriptive_var <- gsub("Body"," Body ", descriptive_var)

colnames(FullDatasetMeanStd) <- descriptive_var


#part 5
library(plyr)
Result <- (ddply(FullDatasetMeanStd, .(Subject_id,activity_id), numcolwise(mean)))
write.table(Result,"temp.txt",row.name=FALSE)

