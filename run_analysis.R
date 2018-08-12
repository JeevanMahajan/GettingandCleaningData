# Author: Jeevan Mahajan
# Date: 11-Aug-2018
# Coursera Course - Getting and Cleaning Data - Final Project

# The Data is downloaded and then loaded into two data sets 'trainingdata' and 'testdata'.

# Load required R packages
library(plyr)
library(dplyr)

# Define Variables.
destFilePath = "C:\\Jeevan\\coursera\\Course3\\data"
projectFileWithPath = paste0(destFilePath,"\\projectdata.zip")
unZippedFolderName = paste0(destFilePath,"\\UCI HAR Dataset")

# Create Data directory if it does not exist already.
if(!file.exists(destFilePath)){dir.create(destFilePath)}

# Download the data if not already downloaded.
if(!file.exists(projectFileWithPath)){
  
# Define Dataset URL.
projectDataSetUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Download the File.
downloadStatus <- download.file(projectDataSetUrl,destfile = projectFileWithPath,method = "curl")
}

# Get the names of the files in the compressed project data file.
if(!file.exists(unZippedFolderName)){
  unzip(projectFileWithPath, exdir = destFilePath)
}

# Read the Training Data Set.
subjectTrainDataSet <- read.table(paste0(unZippedFolderName,"\\train\\subject_train.txt"))
colnames(subjectTrainDataSet) <- "subjectID"
xTrainDataSet <- read.table(paste0(unZippedFolderName,"\\train\\X_train.txt"))
yTrainDataSet <- read.table(paste0(unZippedFolderName,"\\train\\y_train.txt"))
colnames(yTrainDataSet) <- "activityID"

# Read the Test Data Set.
subjectTestDataSet <- read.table(paste0(unZippedFolderName,"\\test\\subject_test.txt"))
colnames(subjectTestDataSet) <- "subjectID"
xTestDataSet <- read.table(paste0(unZippedFolderName,"\\test\\X_test.txt"))
yTestDataSet <- read.table(paste0(unZippedFolderName,"\\test\\y_test.txt"))
colnames(yTestDataSet) <- "activityID"

# Merge Training Data Set.
mergedTrainingDataSet <- cbind(subjectTrainDataSet, yTrainDataSet)
mergedTrainingDataSet <- cbind(mergedTrainingDataSet, xTrainDataSet)

# Merge Test Data Set.
mergedTestDataSet <- cbind(subjectTestDataSet, yTestDataSet)
mergedTestDataSet <- cbind(mergedTestDataSet, xTestDataSet)

# Merge Training and Test Data sets.
mergedTrainingTestDataSet <- rbind(mergedTestDataSet, mergedTrainingDataSet)

#Create a new Data Set with only those columns containing mean or std in column name.
meanStdDataSet <- select(mergedTrainingTestDataSet, "subjectID","activityID", "V1", "V2", "V3", "V4", "V5", "V6", "V41", "V42", "V43", "V44", "V45", "V46", "V81", "V82", "V83", "V84", "V85", "V86", "V121", "V122", "V123", "V124", "V125", "V126", "V161", "V162", "V163", "V164", "V165", "V166", "V201", "V202", "V214", "V215", "V227", "V228", "V240", "V241", "V253", "V254", "V266", "V267", "V268", "V269", "V270", "V271", "V294", "V295", "V296", "V345", "V346", "V347", "V348", "V349", "V350", "V373", "V374", "V375", "V424", "V425", "V426", "V427", "V428", "V429", "V452", "V453", "V454", "V503", "V504", "V513", "V516", "V517", "V526", "V529", "V530", "V539", "V542", "V543", "V552")

# Read Activity Labels.
activityLabels <- read.table(paste0(unZippedFolderName,"\\activity_labels.txt"))

# Update the activity id with Activity name / description.
meanStdDataSet$activityID <- activityLabels[match(meanStdDataSet$activityID, activityLabels$V1), 2]

# Read Featurelist.
featureNames <- read.table(paste0(unZippedFolderName,"\\features.txt"))
featureNames <- mutate(featureNames, refColName = paste0("V", V1))

# Sanitize the variable names.
featureNames$V2 <- sub("t","time", featureNames$V2, ignore.case = FALSE)
featureNames$V2 <- sub("f","frequency", featureNames$V2, ignore.case = FALSE)
featureNames$V2 <- gsub("-",".", featureNames$V2, ignore.case = FALSE)
featureNames$V2 <- gsub("\\(","", featureNames$V2, ignore.case = FALSE)
featureNames$V2 <- gsub(")","", featureNames$V2, ignore.case = FALSE)
featureNames$V2 <- gsub(",",".", featureNames$V2, ignore.case = FALSE)

# Update the variable names to descriptive variable names.
for(i in 3:length(names(meanStdDataSet))){
  colnames(meanStdDataSet)[i] <- featureNames[match(colnames(meanStdDataSet)[i], featureNames$refColName), 2]
}

# Group meanStdDataSet by SubjectID and activity.
meanStdTidyData <- group_by(meanStdDataSet, subjectID, activityID)

# Summarize variable by group.
meanStdTidyData <- summarise_all(meanStdTidyData, mean)

# Store the tidy data into .TXT file with row.names = FALSE.
write.table(meanStdTidyData, paste0(unZippedFolderName,"\\meanStdTidyData.txt"), row.name=FALSE)