
# Course 3 Week 4 
# Getting and Cleaning Data Course Project

## set working directory
wd <- setwd("H:/Documents/DataSciences/Course3week4")


## Downloading data 
if(!file.exists("H:/Documents/DataSciences/Course3week4")){dir.create("H:/Documents/DataSciences/Course3week4/UCI_HAR_Dataset")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "H:/Documents/DataSciences/Course3week4/UCI_HAR_Dataset.zip")

## Unzipping data
    unzip("H:/Documents/DataSciences/Course3week4/UCI_HAR_Dataset.zip", 
          exdir = wd) 

    
## Read and convert data into a single data frame
features <- read.table("UCI HAR Dataset/features.txt", 
                           col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", 
                         col.names = c("code", "activity"))
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt",
                           col.names = "subject")
x.test <- read.table("UCI HAR Dataset/test/X_test.txt", 
                     col.names = features$functions)
y.test <- read.table("UCI HAR Dataset/test/y_test.txt", 
                     col.names = "code")
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt",
                            col.names = "subject")
x.train <- read.table("UCI HAR Dataset/train/X_train.txt",
                      col.names = features$functions)
y.train <- read.table("UCI HAR Dataset/train/y_train.txt", 
                      col.names = "code")
    
## 1. Merges the training and the test sets to create one data set.

X.data <- rbind(x.train, x.test)
Y.data <- rbind(y.train, y.test)
subject <- rbind(subject.train, subject.test)
Merge.Data <- cbind(subject, X.data, Y.data)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

#load package to use %>%
library(dplyr)

#extracting mean and standard deviation measurements from merged data
mean.std.data <- Merge.Data %>% 
    select(subject, code, contains("mean"), contains("std"))

#3.  Uses descriptive activity names to name the activities in the data set
mean.std.data$code <- activities[mean.std.data$code, 2]

#4. Appropriately labels the data set with descriptive variable names.

names(mean.std.data)[2] = "activity"
names(mean.std.data) <- gsub("Acc", "Accelerometer", names(mean.std.data))
names(mean.std.data) <- gsub("BodyBody", "Body", names(mean.std.data))
names(mean.std.data) <- gsub("Gyro", "Gyroscope", names(mean.std.data))
names(mean.std.data) <- gsub("Mag", "Magnitude", names(mean.std.data))
names(mean.std.data) <- gsub("^t", "Time", names(mean.std.data))
names(mean.std.data) <- gsub("^f", "Frequency", names(mean.std.data))
names(mean.std.data) <- gsub("tBody", "TimeBody", names(mean.std.data))
names(mean.std.data) <- gsub("-mean()", "Mean", names(mean.std.data))
names(mean.std.data) <- gsub("-std()", "STD", names(mean.std.data))
names(mean.std.data) <- gsub("-freq()", "Frequency", names(mean.std.data))
names(mean.std.data) <- gsub("angle", "Angle", names(mean.std.data))
names(mean.std.data) <- gsub("gravity", "Gravity", names(mean.std.data))

names(mean.std.data)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Final.Data <- mean.std.data %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))
write.table(Final.Data, "Final_data.txt",
            row.name=FALSE)
