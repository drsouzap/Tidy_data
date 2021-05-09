## 1- Merges the training and the test sets to create one data
# Download files
set.if(file.exists("Tidy_data")){dir.create("Tidy_data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "run_analysis", method = "curl")
unzip("run_analysis")
list.files()

#Reading tables
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt)
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
Subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
Subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
Features <- read.table("./UCI HAR Dataset/features.txt")
Activity_Labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

#Giving names to columns 
colnames(X_train) = Features[,2]
colnames(Y_train) = "Activity_ID"
colnames(Subject_train) = "Subject_ID"
colnames(X_test) = Features[,2]
colnames(Y_test) = "Activity_ID"
colnames(Subject_test) = "Subject_ID"
colnames(Activity_Labels) <- c("Activity_ID", "Activity_Type")

#Merging files
Merged_Train = cbind(Y_train, Subject_train, X_train)
Merged_Test = cbind(Y_test, Subject_test, X_test)
Merged = rbind(Merged_Train, Merged_Test)
View(Merged)


##2- Extracts only the measurements on the mean and standard deviation for each measurement. 
Col_Names = colnames(Merged)
Mean_and_Std = (grepl("Activity_ID", Col_Names) | grepl("Subject_ID", Col_Names) | grepl("mean..", Col_Names) | grepl("std..", Col_Names))


##3- Uses descriptive activity names to name the activities in the data set
Table_Mean_Std <- Merged[, Mean_and_Std == TRUE]
Table_Activity_Labels = merge(Table_Mean_Std, Activity_Labels, by="Activity_ID", all.x = TRUE)
View(Table_Activity_Labels)
Tidy_Data <- Table_Activity_Labels


##4- Appropriately labels the data set with descriptive variable names. 
View(Tidy_Data)
names(Tidy_Data)<-gsub("Acc", "Accelerometer", names(Tidy_Data))
names(Tidy_Data)<-gsub("Gyro", "Gyroscope", names(Tidy_Data))
names(Tidy_Data)<-gsub("BodyBody", "Body", names(Tidy_Data))
names(Tidy_Data)<-gsub("Mag", "Magnitude", names(Tidy_Data))
names(Tidy_Data)<-gsub("^t", "Time", names(Tidy_Data))
names(Tidy_Data)<-gsub("^f", "Frequency", names(Tidy_Data))
names(Tidy_Data)<-gsub("tBody", "TimeBody", names(Tidy_Data))
names(Tidy_Data)<-gsub("-mean()", "Mean", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-std()", "STD", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-freq()", "Frequency", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("angle", "Angle", names(Tidy_Data))
names(Tidy_Data)<-gsub("gravity", "Gravity", names(Tidy_Data))


##5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Sec_Tidy_Data <- Tidy_Data %>%
group_by(Activity_ID, Subject_ID) %>%
summarise_each(funs(mean))
write.table(Sec_Tidy_Data, "./Tidy_Set.txt", row.names = FALSE)
