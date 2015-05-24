## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

require("data.table")
require("reshape2")

train = read.csv("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
train[,562] = read.csv("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
train[,563] = read.csv("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

test = read.csv("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
test[,562] = read.csv("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
test[,563] = read.csv("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

activityLabels = read.csv("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# Read features and make the feature names better suited for R with some substitutions
features = read.csv("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

## 1. Merges the training and the test sets to create one data set.
allData = rbind(train, test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
reqcols <- grep(".*Mean.*|.*Std.*", features[,2])
# First reduce the features table to what we want
features <- features[reqcols,]
# Now add the last two columns (subject and activity)
reqcols <- c(reqcols, 562, 563)
# And remove the unwanted columns from allData
allData <- allData[,reqcols]
## 4. Appropriately labels the data set with descriptive activity names.
colnames(allData) <- c(features$V2, "Activity", "Subject")
colnames(allData) <- tolower(colnames(allData))

currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
  currentActivity <- currentActivity + 1
}

allData$activity <- as.factor(allData$activity)
allData$subject <- as.factor(allData$subject)

tidy_data = aggregate(allData, by=list(activity = allData$activity, subject=allData$subject), mean)
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data [,90] = NULL
tidy_data [,89] = NULL
setwd(getwd("C:/Users/madhan/Documents/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset"))
write.table(tidy_data, file="./tidy_data.txt", sep="\t")