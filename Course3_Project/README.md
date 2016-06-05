# This document describes the processing for the following R script submitted for the 'Getting and Cleaning Data' course project.
# File name:  run_analysis.R 
# Date:       06/05/2016
# Author:     Dan Bretheim

# The project assignment was to create one R script called run_analysis.R that does the following:
# 1. Merges the training and the test sets to create one data set.  
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Processing in run_analysis.R occurs in the following sequence:

# Housekeeping:
#  Clean up RStudio 
#  Set working directory to where the downloaded files have been upzipped
#  Confirm working directory

# Step 1:  Import measurement data files and merge.
#  Import the two measurement data files (X_train.txt, X_test.txt)
#  Concatenate the train and test files

# Step 2:  Retain only the mean and standard deviation measurements. 
#  Import the features file (features.txt)
#  Assign column names (features) 
#  Identify columns containing the mean and std dev which is the subset of interest
#  Delete all non-mean and non-std dev columns from the measurement data 

# Step 3:  Label the activity values.
#  Import the labels files (y_train.txt, y_test.txt)
#  Concatenate the activity label files
#  Join the activity labels with the measurement data
#  Rename the activity value column to "Activity"
#  Import the activity labels file (activity_labels.txt)
#  Assign the activity labels to the measurement file

# Step 4: Label other variables.
#  Import the subject IDs files (subject_train.txt, subject_test.txt)
#  Concatenate the subject files
#  Join the subject IDs with the measurement data
#  Rename the subject ID column to "SubjectID"

# Step 5: Create a summary data set with the average of each variable for each activity and each subject.
#  Delete irrelevant columns

# Step 6:  Write the summary file.
#  The requirement was to specify row.names=FALSE 
#
# END OF README
