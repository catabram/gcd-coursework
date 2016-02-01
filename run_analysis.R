setwd("gcdata-course-project")

#load data
library(httr) 
url <- "https://d396qusza40orc.cloudfront.net/recievedata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "arhiv.zip"
if(!file.exists(file)){
   download.file(url, file)
}

#unzip and create places for files
rawfiles <- "UCI HAR Dataset"
resultfiles <- "results"
if(!file.exists(rawfiles)){
  unzip(file, list = FALSE, overwrite = TRUE)
} 
if(!file.exists(resultfiles)){
  dir.create(resultfiles)
} 

#read data and transform to data frame
tableextraction <- function (filename,cols = NULL){
  print(paste("Getting table:", filename))
  f <- paste(rawfiles,filename,sep="/")
  data <- data.frame()
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  data
}

#run tableextraction
features <- tableextraction("features.txt")

#read data, create database
recievedata <- function(type, features){
  print(paste("Getting data", type))
  subject_data <- tableextraction(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y_data <- tableextraction(paste(type,"/","y_",type,".txt",sep=""),"activity")
  x_data <- tableextraction(paste(type,"/","X_",type,".txt",sep=""),features$V2)
  return (cbind(subject_data,y_data,x_data))
}

#run arecievedata
test <- recievedata("test", features)
train <- recievedata("train", features)

#save results to right place
saveresults <- function (data,name){
  print(paste("Save results:", name))
  file <- paste(resultfiles, "/", name,".csv" ,sep="")
  write.csv(data,file)
}

#Merge train and test sets
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)

#Extract only measurements on the mean and standard deviation for each measurement. 
mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveresults(mean_and_std,"mean_and_std")

#Use descriptive activity names to name the activities in the data set
activity_labels <- tableextraction("activity_labels.txt")

#Appropriately label the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saveresults(tidy_dataset,"tidy_dataset")