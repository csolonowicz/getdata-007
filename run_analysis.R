# You should create one R script called run_analysis.R that does the following. 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Here are the data for the project: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 


# Set current working directory to where the "UCI HAR Dataset" folder is located at
setwd("C:/Users/Admin/Dropbox/My/Coursera/getdata-007/PeerAss/getdata_projectfiles_UCI HAR Dataset/")

# Read activity labels
activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# Read and Join followin training columns of data
# - 'train/X_train.txt': Training set.
# - 'train/y_train.txt': Training labels.
# - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

training = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)

training <- cbind(training,read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE))
extracol1 <- as.numeric(length(training))  # get the position of activity col

training <- cbind(training,read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE))
extracol2 <- as.numeric(length(training)) # get the position of subject col

# Read and Join followin testing columns of data
# - 'test/X_test.txt': Training set.
# - 'test/y_test.txt': Training labels.
# - 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
testing = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testing <- cbind(testing,read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE))
testing <- cbind(testing,read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE))


# Read features
features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)

# Join training and test sets
data = rbind(training, testing)

# Get only the data on mean and std. dev.
colsWeWant <- grep("*mean*|*std*", features[,2],ignore.case = TRUE)

# Select columns we want
features <- features[colsWeWant,]

# Add subject and activity columns
colsWeWant <- c(colsWeWant, extracol1, extracol2)

# Subset the data we want
data <- data[,colsWeWant]

# Add the column names (features) to data
colnames(data) <- c(as.character(features$V2), "Activity", "Subject")
colnames(data) <- tolower(colnames(data))

currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  data$activity <- gsub(currentActivity, currentActivityLabel, data$activity)
  currentActivity <- currentActivity + 1
}

data$activity <- as.factor(data$activity)
data$subject <- as.factor(data$subject)

# Compute mean over everything except subject and activity
tidy = aggregate(data[,1:(length(data)-2)], by=list(activity = data$activity, subject=data$subject), mean)

write.table(tidy, "tidy.txt", sep="\t")
