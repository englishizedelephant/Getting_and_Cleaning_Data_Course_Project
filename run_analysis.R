# 1. Merges the training and the test sets to create one data set.

## load the dplyr package
library(dplyr)

## download the dataset
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "run_analysis.zip"
download.file(url, destfile)
unzip(destfile)

## create variables for each data
testx <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
testy <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")
testsubject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "")

trainx <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
trainy <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "")
trainsubject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "")

## Combine test and train datasets to create one complete dataset
x <- bind_rows(testx, trainx)
y <- bind_rows(testy, trainy)
subject <- bind_rows(testsubject, trainsubject)
dat <- bind_cols(x, y, subject)

head(dat)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

## Assign column names to the dataset
colnames(dat) <- paste0("", seq_len(ncol(dat)))

## Read the features dataset to get column names
features <- read.table("UCI HAR Dataset/features.txt")

## Get the indices of the columns that include "mean" or "std" in their names
mean_std_No <- grep("[^a-zA-Z]mean[^a-zA-Z]|[^a-zA-Z]std[^a-zA-Z]", features$V2)

## Extract only the columns that include measurements of mean and standard deviation
mean_std_dat <- dat[, c(mean_std_No, ncol(dat)-1, ncol(dat))]
head(mean_std_dat)

# 3. Uses descriptive activity names to name the activities in the data set

## Read the activity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
labels <- setNames(activity_labels$V2, activity_labels$V1)

## Convert the numeric activity values to descriptive activity names
mean_std_dat[, ncol(mean_std_dat)-1] <-
  labels[mean_std_dat[, ncol(mean_std_dat)-1]]

head(mean_std_dat)

# 4. Appropriately labels the data set with descriptive variable names. 

## Create a named vector of descriptive variable names
featuresList <- setNames(features$V2, features$V1)

## Rename the columns of the dataset with descriptive variable names
colnames(mean_std_dat) <- featuresList[colnames(mean_std_dat)]
colnames(mean_std_dat)[ncol(mean_std_dat)-1] <- "activity"
colnames(mean_std_dat)[ncol(mean_std_dat)] <- "subject"

head(mean_std_dat)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Group and summarize the dataset by activity to create a tidy dataset with the average of each variable 
summary <- mean_std_dat %>%
  group_by(subject, activity) %>%
  summarize(across(everything(), mean))
summary

write.table(summary, file = "summary.txt")
