## Script:  run_analysis.R
## Project: Getting and Cleaning Data
## Date:    06-04-2016

## Housekeeping
#  Clean up RStudio
rm(list=ls())
#  Set working directory where the downloaded files have been unzipped
setwd("C:/JHU_R/data")
#  Confirm working directory
getwd()

## Step 1:  Import measurement data files and merge.
#  Import measurement data files
x_train <- read.table("X_train.txt")
x_test <- read.table("X_test.txt")
#  Concatenate the train and test files
x_data <- rbind(x_train,x_test)

## Step 2:  Retain only the mean and std dev measurements.
#  Import features
features <- read.table("features.txt")
#  Assign column names (features) to x_data
colnames(x_data) = features[,2]
#  Identify columns containing the mean and std which is the subset of interest
meanandstd <- grep("-(mean|std)\\(\\)", features[,2])
#  Delete all non-mean and non-std dev columns from x_data
x_data <- x_data[,meanandstd]

## Step 3:  Label the activity values.    
#  Import label files
y_train <- read.table("y_train.txt")
y_test <- read.table("y_test.txt")
#  Concatenate the activity label files
y_data <- rbind(y_train,y_test)
#  Join the activity labels with the measurement data
xy_data <- cbind(y_data,x_data)
#  Rename activity value
names(xy_data) [1] <- "Activity"
#  Import activity labels
activitylabels <- read.table("activity_labels.txt")
#  Assign activity label
xy_data[,1] <- activitylabels[xy_data[,1],2]

## Step 4:  Label other variables.
#  Import subject IDs
subject_train <- read.table("subject_train.txt")
subject_test <- read.table("subject_test.txt")
#  Contatenate the subject files
subject_data <- rbind(subject_train,subject_test)
#  Join the subject IDs with the measurement data  
all_data <- cbind(subject_data,xy_data)
# Rename subject ID
names(all_data) [1] <- "SubjectID"

## Step 5:  Create a summary data set with the average of each variable
#           for each activity and each subject.
tidy = aggregate(all_data, by=list(Activity = all_data$Activity, SubjectID=all_data$SubjectID), mean)
#  Delete irrelevant columns
tidy[3:4] <- list(NULL)

## Step 6:  Write the summary file.
write.table(tidy, "tidy.txt", row.names=FALSE, quote=FALSE, sep="\t")





