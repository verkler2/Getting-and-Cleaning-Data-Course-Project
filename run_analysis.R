##load library
library(dplyr)

# Check to see if file exists
if(!file.exists("./data")){dir.create("./data")}
# download data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")
# unzip the zipped data
unzip(zipfile="./data/Dataset.zip",exdir="./data")
# Unzip dataset
unzip(zipfile = "./getcleandata/projectdataset.zip", exdir = "./getcleandata")

#STEP 1: Merges the training and the test sets to create one data set
#Read training datasets
x_train <- read.table("./getcleandata/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./getcleandata/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./getcleandata/UCI HAR Dataset/train/subject_train.txt")

#Read test datasets
x_test <- read.table("./getcleandata/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./getcleandata/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./getcleandata/UCI HAR Dataset/test/subject_test.txt")

# Merge training and test data sets
x <- rbind(x_train,x_test)
# Assign columns for subjects and activities
subject <- rbind(subject_train, subject_test)
y <- rbind(y_train, y_test)
data <- cbind(subject, y, x)
# assign column names for merged data set
features <- read.table("./data/UCI HAR Dataset/features.txt")
features<-as.vector(features[,2])
names(data) <- c("subject", "activity", features)

##STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement
meanStdCols <- grep("[mM]ean.*\\(\\)|[sS]td.*\\(\\)",names(data))
selectedData <- data[c(1,2,meanStdCols)]
columns <- gsub("-","",names(selectedData))
columns <- gsub("mean","Mean",columns)
columns <- gsub("std","Std",columns)
columns <- gsub("\\(\\)","",columns)
names(selectedData) <- columns

##STEP 3: Uses descriptive activity names to name the activities in the data set
selectedData <- mutate(selectedData, activity = factor(activity, 1:6, labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")))

##STEP 4: Appropriately labels the data set with descriptive variable names
##group by subjects and activities
grouped_selectedData <- group_by(selectedData, subject, activity)

# summarize the average of each variable for each activity and subject
summaryAvgData <- summarize_each(grouped_selectedData, funs(mean))

# convert to data frame.
summaryAvgData_df <- as.data.frame(summaryAvgData)

##STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
write.table(summaryAvgData_df, "summaryAvgData.txt", row.name=FALSE)
