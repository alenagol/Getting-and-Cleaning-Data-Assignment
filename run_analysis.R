run_analysis <- function(){
  setwd("UCI HAR Dataset")
  # load test data  
  subject_test = read.table("test/subject_test.txt")
  X_test = read.table("test/X_test.txt")
  Y_test = read.table("test/Y_test.txt")
  
  # load training data
  subject_train = read.table("train/subject_train.txt")
  X_train = read.table("train/X_train.txt")
  Y_train = read.table("train/Y_train.txt")
  
  # load lookup information
  features <- read.table("features.txt", col.names=c("featureId", "featureLabel"))
  activities <- read.table("activity_labels.txt", col.names=c("activityId", "activityLabel"))
  activities$activityLabel <- gsub("_", "", as.character(activities$activityLabel))
  includedMean <- grep("mean\\(\\)", features$featureLabel)
  includedStd <- grep("std\\(\\)", features$featureLabel)
  includedAll<-c(includedMean,includedStd)
  # merge test and training data and then name them
  subject <- rbind(subject_test, subject_train)
  names(subject) <- "subjectId"
  X <- rbind(X_test, X_train)
  X <- X[, includedAll]
  names(X) <- gsub("\\(|\\)", "", features$featureLabel[includedAll])
  Y <- rbind(Y_test, Y_train)
  names(Y) = "activityId"
  activity <- merge(Y, activities, by="activityId")$activityLabel
  setwd("..")
  # merge data frames of different columns to form one data table
  data <- cbind(subject, X, activity)
  write.table(data, "merged_tidy_data.txt")
  
  # create a dataset grouped by subject and activity after applying standard deviation and average calculations
  library(data.table)
  dataDT <- data.table(data)
  calculatedData<- dataDT[, lapply(.SD, mean), by=c("subjectId", "activity")]
  write.table(calculatedData, "calculated_tidy_data.txt")
}

