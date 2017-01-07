## Set the working directory
setwd("~/Documenten/R_DOCUMENTEN/Coursera_Repo")

## load necessary packages
library(data.table)

## TASK 1: MERGE THE TRAINING AND TEST SETS TO CREATE ONE DATA SET
## ____________________________________________________________________

## load the train data in 3 tables
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
labelsTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
dataTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

## load the test data in 3 tables
subjectTest <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
labelsTest <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
dataTest <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

## merge the 3 tables by row binding
subject <- rbind(subjectTrain, subjectTest)
labels <- rbind(labelsTrain, labelsTest)
data <- rbind(dataTrain, dataTest)

## set the variable names of subject and labels
setnames(subject, "V1", "subject")
setnames(labels, "V1", "activity")

## set the variable names of the data
# load the variable names
features <- read.table("UCI HAR Dataset/features.txt",head=FALSE)
# set the variable names
names(data) = features$V2

## merge the datasets
allDataSubject_Label <- cbind(subject,labels)
allData <- cbind(allDataSubject_Label,data)

## TASK 2: EXTRACT ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION
## _____________________________________________________________________

## define which columns have the word mean() or std() in them
subsetfeatures <- features$V2[grep("mean\\(\\)|std\\(\\)",features$V2)]

## subset 'data' leaving the names from subfeatures, the subject and the activity
columnnames <- c(as.character(subsetfeatures),"subject","activity")
allData_mean_std <- subset(allData,select=columnnames)

## TASK 3: name the activities (descriptive) in the dataset
## _________________________________________________________

# read the descriptive activity names from activity_labels.txt
activityNames <- read.table("UCI HAR Dataset/activity_labels.txt",head=FALSE)
# name the variables to join them with the allData_mean_stad table
setnames(activityNames,names(activityNames),c("activity","description"))

# merge the two files on activity
allData_mean_std <- merge(activityNames,allData_mean_std,by="activity",all.x = TRUE)

# change some headers
setnames(allData_mean_std, "activity", "activity_id")
setnames(allData_mean_std, "description", "activity")

#factorize the activity column
allData_mean_std$activity <- factor(allData_mean_std$activity)

## TASK 4: label the dataset with descriptive variable names
## ______________________________________________________________

## when the variablename start with a f it means frequency
names(allData_mean_std) <- gsub("^f","Frequency",names(allData_mean_std))
##  Acc = Accelerator
names(allData_mean_std) <- gsub("Acc","Accelerator",names(allData_mean_std))
## Mag = Magnitude
names(allData_mean_std) <- gsub("Mag","Magnitude",names(allData_mean_std))
## Gyro = Gyroscope
names(allData_mean_std) <- gsub("Gyro","Gyroscope",names(allData_mean_std))

## TASK 5: create a second independent tidy dataset with the average for each variable, for each activity for each subject
## _______________________________________________________________________________________________________________________
# first also factorize subject
allData_mean_std$subject <- factor(allData_mean_std$subject)

# transform allData_mean_std to a data table
allData_mean_std <- data.table(allData_mean_std)

# create the new tidy dataset with means for alle variables per subject and activity
newDataSet <- aggregate(. ~subject + activity, allData_mean_std, mean)

# write to a file
write.table(newDataSet,"newTidyDataset.txt",row.name=FALSE)


