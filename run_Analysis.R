#1.Merges the training and the test sets to create one data set.

library(data.table)

# downloading and unzipping

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "temp.zip")
unzip("temp.zip",exdir = "temp2")

# From readme.txt 

#features_info.txt': Shows information about the variables used on the feature vector.
#'features.txt': List of all features.
#'activity_labels.txt': Links the class labels with their activity name.
#'train/X_train.txt': Training set.
#'train/y_train.txt': Training labels.
#'test/X_test.txt': Test set.
#'test/y_test.txt': Test labels.


# read test data
Xtest <- read.table("./temp2/UCI HAR Dataset/test/X_test.txt")
Ytest <- read.table("./temp2/UCI HAR Dataset/test/y_test.txt")
subjectTest <- read.table("./temp2/UCI HAR Dataset/test/subject_test.txt")



# read train data
Xtrain <- read.table("./temp2/UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("./temp2/UCI HAR Dataset/train/y_train.txt")
subjectTrain <- read.table("./temp2/UCI HAR Dataset/train/subject_train.txt")


# general data

activity_label <- read.table("./temp2/UCI HAR Dataset/activity_labels.txt")
features <- read.table("./temp2/UCI HAR Dataset/features.txt")


# giving names

names(Xtrain) = features$V2
names(Xtest) = features$V2


names(Ytrain) ="ID"
names(Ytest) = "ID"


Ytest= merge(Ytest,activity_label,by.x="ID",by.y="V1",all.x= TRUE)

Ytrain= merge(Ytrain,activity_label,by.x="ID",by.y="V1",all.x= TRUE)


#Collating Train
X = cbind(Xtrain,Ytrain,subjectTrain)

#Collating Test
Y = cbind(Xtest,Ytest,subjectTest)

# Collating Final data

finalData = rbind(X,Y)


#2.Extracts only the measurements on the mean and standard deviation for each measurement.

#extracting required columns

Mean_std_id <- (grepl("ID", names(finalData)) | grepl("V2", names(finalData)) | 
    grepl("-std()", names(finalData)) | grepl("-mean()",names(finalData)) )

finalData = finalData[,Mean_std_id]


names(finalData)[81]="ID-EXP"

#3.Uses descriptive activity names to name the activities in the data set

# done while merging the tables before collating to one dataframe.

#4.Appropriately labels the data set with descriptive variable names.

names(finalData)

names(finalData) = gsub("mean","Mean ",names(finalData))
names(finalData) = gsub("std","Standard Dev",names(finalData))
names(finalData) = gsub("[()]","",names(finalData))

#5.From the data set in step 4, creates a second, independent tidy data set with 
  #the average of each variable for each activity and each subject.


# aggregating by the ID and acitivty(ID-EXP)
Q5 <- aggregate(finalData,
                  by = list(finalData$ID, finalData$`ID-EXP`), mean)

names(Q5)[names(Q5)=="Group.1"] = "Group by Id"
names(Q5)[names(Q5)=="Group.2"] = "Group by Activity"

#writing output
write.table(Q5, file = "Q5output", row.names = FALSE, sep = ",")

# Q5 has the required df with the grouping .
