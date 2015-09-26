# Set the name of my working directorie and file names

directory = paste("/Users/dmzherlitsyn/GitHub/datasciencecoursera/Course 3 Project/UCI HAR Dataset","/", sep="")
fileset1 = c("test/subject_test.txt","test/X_test.txt", "test/y_test.txt")
fileset2 = c("train/subject_train.txt","train/X_train.txt", "train/y_train.txt")

# Read the attribute data and labels
activity_labels <- read.table(paste(directory,"activity_labels.txt", sep=""))
features <- read.table(paste(directory,"features.txt", sep=""))

# Make the working function "Merges". "Merges" func. merges the data to the one data set with the activities labels and subject numbers.
Merges <- function (directory, activity_labels, features, files) {
  # Reads main data set table in the "data" variable (fread() - makes error)
  data <- read.table(paste(directory,files[2], sep=""))
  # Checks for the duplicate labels in the Features 
  valid_col_names <- make.names(names=features$V2, unique=TRUE, allow_ = TRUE) 
  # Makes the column labels
  names(data) <- valid_col_names
  # Reads the Subject and Activity names for each row. Adds the new columns
  subj <- read.table(paste(directory,files[1], sep=""))
  data$subject <- factor(subj$V1)
  y <- read.table(paste(directory,files[3], sep="")) 
  data$activity <- factor(y$V1, labels=activity_labels$V2)
  # Returns the merged data set
  data
}

# 1. Merges the training and the test sets to create one data set. 
# 3. Uses descriptive activity names to name the activities in the data set

data <- rbind(Merges(directory, activity_label, features, fileset1), Merges(directory, activity_label, features, fileset2))

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

library(dplyr)
mydata <- select(data,contains("mean.."),contains("std.."),matches("activity"),matches("subject"))
numvars <- ncol(mydata)-2

# 4. Appropriately labels the data set with descriptive variable names.
library(reshape2)
melt_data <- melt(mydata, id=c("subject","activity"), measure.vars=c(1:numvars))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
out_data <- dcast(melt_data, subject + activity ~ variable, mean)

# Writes the final tidy data in the "out_data.txt" file
write.table(out_data, file=paste(directory,"out_data.txt", sep=""), row.name = FALSE)