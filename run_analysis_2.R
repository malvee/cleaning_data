setwd("C:\\Users\\Momshad\\Downloads\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset")

# 1. Merges the training and the test sets to create one data set.

# Read in the data from base files
features     = read.table('./features.txt',header=FALSE);
activity_table = read.table('./activity_labels.txt',header=FALSE);

# Read in training data
train_subjects <- read.table('./train/subject_train.txt',header=FALSE);
train_x <-read.table('./train/x_train.txt',header=FALSE);
train_y <- read.table('./train/y_train.txt',header=FALSE);

# Read in the test data
test_subjects <- read.table('./test/subject_test.txt',header=FALSE);
test_x <- read.table('./test/x_test.txt',header=FALSE);
test_y <- read.table('./test/y_test.txt',header=FALSE);

# Assign meaningful variable names to the data
colnames(activity.table)   <- c('activity_id','activity_type');

colnames(train_subjects) <- "subject_id";
colnames(train_x)        <- features[,2]; 
colnames(train_y)        <- "activity_id";

colnames(test_subjects) <- "subject_id";
colnames(test_x)        <- features[,2]; 
colnames(test_y)        <- "activity_id"

# Create the final training set by merging train.subjects, train.x and train.y
data_training = cbind(train_subjects, train_y, train_x);

# Create the final test set by merging test.subjects, test.x and test.y
data_test = cbind(test_subjects, test_y, test_x);

# Combine training and test data to create a final data set
data_final = rbind(data_training, data_test);

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

colNames <- colnames(data_final)

# Extracts only the columns which has "-mean" or "-std" in the end. "-meanFreq" columns are not included.
log_vec = (grepl("subject..", colNames) | grepl("activity..", colNames) | grepl("-mean..", colNames) & !grepl("-meanFreq..",colNames) | grepl("-std..", colNames));
data_final = data.final[,log_Vector]

# 3. Use descriptive activity names to name the activities in the data set

data_final = merge(data_final, activity_table, by.x="activity_id", by.y="activity_id", all.x=TRUE);

# 4. Appropriately label the data set with descriptive variable names. 

colNames <- colnames(data_final)

colNames <- gsub("-", ".", colNames)
colNames <- gsub("\\()", "", colNames)
colNames <- gsub("BodyBody", "body.", colNames)
colNames <- gsub("Body", "body.", colNames)
colNames <- gsub("Acc", "accelerometer.", colNames)
colNames <- gsub("Gyro", "gyroscope.", colNames)
colNames <- gsub("Mag", "magnitude.", colNames)
colNames <- gsub("Jerk", "jerk.", colNames)
colNames <- gsub("Gravity", "gravity.", colNames)
colNames <- gsub("^(t)", "time.", colNames)
colNames <- gsub("^(f)", "frequency.", colNames)
colNames <- gsub("..", ".", colNames, fixed=TRUE)

colnames(data_final) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

ids = c("activity_id", "activity_type", "subject_id")
data_tidy = aggregate(data_final[, !(names(data_final) %in% ids)], by=list(subject_id = data_final$subject_id, activity.type = data_final$activity_type), mean);

write.table(data.tidy, '../tidy_data.txt');
