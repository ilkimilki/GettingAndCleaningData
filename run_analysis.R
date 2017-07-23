  ## Load required libraries

library(data.table)
library(dplyr)

  ## Download and unzip data

if(!file.exists("./data")){dir.create("./data")}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(url, "./data/Data.zip", mode="wb")
unzip(zipfile="./data/Data.zip",exdir="./data")

  ## Read in the data

data_path <-file.path("./data/UCI HAR Dataset")

x_test <- fread(file.path(data_path,"test", "X_test.txt"))
df_test <- data.table(x_test)
x_train <- fread(file.path(data_path,"train", "X_train.txt"))
df_train <- data.table(x_train)

  # Bind the training and test datasets
dt <- rbind(df_test, df_train)

  ## Add the subject and activity IDs to the combined dataset
 
subject_test <- fread(file.path(data_path, "test",  "subject_test.txt"))
subject_train <- fread(file.path(data_path, "train",  "subject_train.txt"))
subject <-rbind(subject_test, subject_train)
subject <- setnames(subject, "V1", "subject")

activity_labels_test <- fread(file.path(data_path, "test",  "y_test.txt"))
activity_labels_train <- fread(file.path(data_path, "train",  "y_train.txt"))
activity_labels <- rbind(activity_labels_test, activity_labels_train)
activity_labels <-setnames(activity_labels, "V1", "activity")

subject <- cbind(subject, activity_labels)

dt <- cbind(subject, dt)
 
 # set keys for subject and activity
setkey(dt, subject, activity)

  ## Select mean and std colums for each variables in dt
  
# read in features
features <-fread(file.path(data_path, "features.txt"))
setnames(features, names(features), c("featureIndex", "featureName"))

  # Select only features that contain "mean()" or "std()" in feature name and tidy names
features_mean_std <- features[grepl("mean\\(\\)|std\\(\\)", features$featureName)]

features_mean_std$featureName <- sub("\\(\\)", "", features_mean_std$featureName)
features_mean_std$featureName <- gsub("\\-", "", features_mean_std$featureName)
features_mean_std$featureName <- tolower(features_mean_std$featureName)
  
# Iterate over featureNumbers to select colums in dt
dt <- dt[,(c(key(dt), paste0("V", features_mean_std$featureIndex))), with=F]

  ## read in activity labels and assign activity names in dt
ActivityNames <- fread(file.path(data_path, "activity_labels.txt"))
setnames(ActivityNames, names(ActivityNames), c("activity", "activityName"))
dt <- merge(dt, ActivityNames, by="activity", all.x=T)

setkey(dt, subject, activity, activityName)

  ## Melt dt to generate table with and all variables condensed into one columns
dt <- data.table(melt(dt, key(dt), variable.name="featureVnumber"))
dt <- select(dt, subject,  activityName, featureVnumber, value)
setkey(dt, subject,  activityName, featureVnumber)

  # generate V-numbers from featureIndex column in features_mean_std 
  # and rename to merge with dt on featureVnumber column

features_mean_std$featureIndex <-paste0("V",features_mean_std$featureIndex)

colnames(features_mean_std)[1] <- "featureVnumber"

dt <- merge(dt, features_mean_std, by="featureVnumber", all.x=T)
setkey(dt, subject,  activityName, featureName)

tidy_dt <-dt[, list( average = mean(value)), by=key(dt)]
names(tidy_dt) <-tolower(names(tidy_dt))

write.table(tidy_dt, "tidy.txt", row.names = F)
