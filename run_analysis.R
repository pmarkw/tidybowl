# Getting and Cleaning Data
# Course Project

# To use this script, create a working directory
# and unzip the getdata-projectfiles-UCI HAR Dataset.zip
# file into directory UCI HAR Dataset under
# your working directory.  Then change the directory
# in setwd below to your directory

setwd("C:/PMW/MyWorking.pmw/Coursera/GCD_Prj")
library(dplyr)


# Get feature labels for column names
colNameTab <- read.table("UCI HAR Dataset/features.txt")
colNames <- as.character(colNameTab$V2)

# Get activity descriptive lables
actNamesTab <- read.table("UCI HAR Dataset/activity_labels.txt")

# Load Train set
trainSubj <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names="Subject")

trainFeature <- read.table("UCI HAR Dataset/train/X_train.txt", col.names=colNames)

trainActTab <- read.table("UCI HAR Dataset/train/y_train.txt")
trainActMerge <- merge(trainActTab, actNamesTab)
trainAct <- data.frame(trainActMerge[,"V2"])
names(trainAct) <- "Activity"
trainData <- cbind(trainAct,trainSubj,trainFeature)

# Cleanup
trainAct = 0
trainActMerge = 0
trainActTab = 0
trainFeature = 0
trainSubj = 0
gc()

# Load Test set
testSubj <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names="Subject")

testFeature <- read.table("UCI HAR Dataset/test/X_test.txt", col.names=colNames)

testActTab <- read.table("UCI HAR Dataset/test/y_test.txt")
testActMerge <- merge(testActTab, actNamesTab)
testAct <- data.frame(testActMerge[,"V2"])
names(testAct) <- "Activity"
testData <- cbind(testAct,testSubj,testFeature)

# Cleanup
testAct = 0
testActMerge = 0
testActTab = 0
testFeature = 0
testSubj = 0
gc()

# Make tidy dataset
totalData <- rbind(testData, trainData)
meanStdData <- select(totalData, matches("Activity"), matches("Subject"), contains("mean"), contains("std"))

# Cleanup
testData = 0
trainData = 0
totalData = 0
gc()

avgMeanStdData <- summarize(group_by(meanStdData, Activity, Subject),
                            tBodyAcc.mean...X=mean(tBodyAcc.mean...X), 
                            tBodyAcc.mean...Y=mean(tBodyAcc.mean...Y), 
                            tBodyAcc.mean...Z=mean(tBodyAcc.mean...Z), 
                            tGravityAcc.mean...X=mean(tGravityAcc.mean...X), 
                            tGravityAcc.mean...Y=mean(tGravityAcc.mean...Y), 
                            tGravityAcc.mean...Z=mean(tGravityAcc.mean...Z), 
                            tBodyAccJerk.mean...X=mean(tBodyAccJerk.mean...X), 
                            tBodyAccJerk.mean...Y=mean(tBodyAccJerk.mean...Y), 
                            tBodyAccJerk.mean...Z=mean(tBodyAccJerk.mean...Z), 
                            tBodyGyro.mean...X=mean(tBodyGyro.mean...X), 
                            tBodyGyro.mean...Y=mean(tBodyGyro.mean...Y), 
                            tBodyGyro.mean...Z=mean(tBodyGyro.mean...Z), 
                            tBodyGyroJerk.mean...X=mean(tBodyGyroJerk.mean...X), 
                            tBodyGyroJerk.mean...Y=mean(tBodyGyroJerk.mean...Y), 
                            tBodyGyroJerk.mean...Z=mean(tBodyGyroJerk.mean...Z), 
                            tBodyAccMag.mean..=mean(tBodyAccMag.mean..), 
                            tGravityAccMag.mean..=mean(tGravityAccMag.mean..), 
                            tBodyAccJerkMag.mean..=mean(tBodyAccJerkMag.mean..), 
                            tBodyGyroMag.mean..=mean(tBodyGyroMag.mean..), 
                            tBodyGyroJerkMag.mean..=mean(tBodyGyroJerkMag.mean..), 
                            fBodyAcc.mean...X=mean(fBodyAcc.mean...X), 
                            fBodyAcc.mean...Y=mean(fBodyAcc.mean...Y), 
                            fBodyAcc.mean...Z=mean(fBodyAcc.mean...Z), 
                            fBodyAcc.meanFreq...X=mean(fBodyAcc.meanFreq...X), 
                            fBodyAcc.meanFreq...Y=mean(fBodyAcc.meanFreq...Y), 
                            fBodyAcc.meanFreq...Z=mean(fBodyAcc.meanFreq...Z), 
                            fBodyAccJerk.mean...X=mean(fBodyAccJerk.mean...X), 
                            fBodyAccJerk.mean...Y=mean(fBodyAccJerk.mean...Y), 
                            fBodyAccJerk.mean...Z=mean(fBodyAccJerk.mean...Z), 
                            fBodyAccJerk.meanFreq...X=mean(fBodyAccJerk.meanFreq...X), 
                            fBodyAccJerk.meanFreq...Y=mean(fBodyAccJerk.meanFreq...Y), 
                            fBodyAccJerk.meanFreq...Z=mean(fBodyAccJerk.meanFreq...Z), 
                            fBodyGyro.mean...X=mean(fBodyGyro.mean...X), 
                            fBodyGyro.mean...Y=mean(fBodyGyro.mean...Y), 
                            fBodyGyro.mean...Z=mean(fBodyGyro.mean...Z), 
                            fBodyGyro.meanFreq...X=mean(fBodyGyro.meanFreq...X), 
                            fBodyGyro.meanFreq...Y=mean(fBodyGyro.meanFreq...Y), 
                            fBodyGyro.meanFreq...Z=mean(fBodyGyro.meanFreq...Z), 
                            fBodyAccMag.mean..=mean(fBodyAccMag.mean..), 
                            fBodyAccMag.meanFreq..=mean(fBodyAccMag.meanFreq..), 
                            fBodyBodyAccJerkMag.mean..=mean(fBodyBodyAccJerkMag.mean..), 
                            fBodyBodyAccJerkMag.meanFreq..=mean(fBodyBodyAccJerkMag.meanFreq..), 
                            fBodyBodyGyroMag.mean..=mean(fBodyBodyGyroMag.mean..), 
                            fBodyBodyGyroMag.meanFreq..=mean(fBodyBodyGyroMag.meanFreq..), 
                            fBodyBodyGyroJerkMag.mean..=mean(fBodyBodyGyroJerkMag.mean..), 
                            fBodyBodyGyroJerkMag.meanFreq..=mean(fBodyBodyGyroJerkMag.meanFreq..), 
                            angle.tBodyAccMean.gravity.=mean(angle.tBodyAccMean.gravity.), 
                            angle.tBodyAccJerkMean..gravityMean.=mean(angle.tBodyAccJerkMean..gravityMean.), 
                            angle.tBodyGyroMean.gravityMean.=mean(angle.tBodyGyroMean.gravityMean.), 
                            angle.tBodyGyroJerkMean.gravityMean.=mean(angle.tBodyGyroJerkMean.gravityMean.), 
                            angle.X.gravityMean.=mean(angle.X.gravityMean.), 
                            angle.Y.gravityMean.=mean(angle.Y.gravityMean.), 
                            angle.Z.gravityMean.=mean(angle.Z.gravityMean.), 
                            tBodyAcc.std...X=mean(tBodyAcc.std...X), 
                            tBodyAcc.std...Y=mean(tBodyAcc.std...Y), 
                            tBodyAcc.std...Z=mean(tBodyAcc.std...Z), 
                            tGravityAcc.std...X=mean(tGravityAcc.std...X), 
                            tGravityAcc.std...Y=mean(tGravityAcc.std...Y), 
                            tGravityAcc.std...Z=mean(tGravityAcc.std...Z), 
                            tBodyAccJerk.std...X=mean(tBodyAccJerk.std...X), 
                            tBodyAccJerk.std...Y=mean(tBodyAccJerk.std...Y), 
                            tBodyAccJerk.std...Z=mean(tBodyAccJerk.std...Z), 
                            tBodyGyro.std...X=mean(tBodyGyro.std...X), 
                            tBodyGyro.std...Y=mean(tBodyGyro.std...Y), 
                            tBodyGyro.std...Z=mean(tBodyGyro.std...Z), 
                            tBodyGyroJerk.std...X=mean(tBodyGyroJerk.std...X), 
                            tBodyGyroJerk.std...Y=mean(tBodyGyroJerk.std...Y), 
                            tBodyGyroJerk.std...Z=mean(tBodyGyroJerk.std...Z), 
                            tBodyAccMag.std..=mean(tBodyAccMag.std..), 
                            tGravityAccMag.std..=mean(tGravityAccMag.std..), 
                            tBodyAccJerkMag.std..=mean(tBodyAccJerkMag.std..), 
                            tBodyGyroMag.std..=mean(tBodyGyroMag.std..), 
                            tBodyGyroJerkMag.std..=mean(tBodyGyroJerkMag.std..), 
                            fBodyAcc.std...X=mean(fBodyAcc.std...X), 
                            fBodyAcc.std...Y=mean(fBodyAcc.std...Y), 
                            fBodyAcc.std...Z=mean(fBodyAcc.std...Z), 
                            fBodyAccJerk.std...X=mean(fBodyAccJerk.std...X), 
                            fBodyAccJerk.std...Y=mean(fBodyAccJerk.std...Y), 
                            fBodyAccJerk.std...Z=mean(fBodyAccJerk.std...Z), 
                            fBodyGyro.std...X=mean(fBodyGyro.std...X), 
                            fBodyGyro.std...Y=mean(fBodyGyro.std...Y), 
                            fBodyGyro.std...Z=mean(fBodyGyro.std...Z), 
                            fBodyAccMag.std..=mean(fBodyAccMag.std..), 
                            fBodyBodyAccJerkMag.std..=mean(fBodyBodyAccJerkMag.std..), 
                            fBodyBodyGyroMag.std..=mean(fBodyBodyGyroMag.std..), 
                            fBodyBodyGyroJerkMag.std..=mean(fBodyBodyGyroJerkMag.std..)
                            )


write.table(avgMeanStdData, file = "avgMeanStdData.txt", row.names = FALSE)

