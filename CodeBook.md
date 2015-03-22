###Primarily made use of sapply functions to do the necessary variable transforms. ddply to get the means for part 5. Variable name transforms were done with the following lines of code:

descriptive_var <- gsub("mean\\(\\)"," Mean Value ", descriptive_var)

descriptive_var <- gsub("std\\(\\)"," standard deviation ", descriptive_var)

descriptive_var <- gsub("mean"," Mean Value ", descriptive_var)

descriptive_var <- gsub("std"," standard deviation ", descriptive_var)

descriptive_var <- gsub("^f"," Freq Domain ", descriptive_var)

descriptive_var <- gsub("^t"," Time Domain ", descriptive_var)

descriptive_var <- gsub("Acc"," Acceleration ", descriptive_var)

descriptive_var <- gsub("Body"," Body ", descriptive_var)

###Please check ReadMe for line by line description.
