# Below script Merges training data and test data to create single data set

# Install the pacman packate

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(data.table, reshape2, gsubfn)

# Get data from the zip file from the url provided

path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "data.zip"))
unzip(zipfile = "data.zip")

# Load the activity Labels
activityLabels <- fread(
  file.path(path, "UCI HAR Dataset/activity_labels.txt"),
  col.names = c("classLabels", "activityNames")
)

# Load the features

features <-fread(
  file.path(path, "/UCI HAR Dataset/features.txt"),
  col.names = c("index", "featureNames")
)

# from the features extract mean and standard deviation
# use gsubfn to extract data with the feature Names
featuresNeeded <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresNeeded, featureNames]
measurements <- gsubfn(
  "(^t|^f|Acc|Gyro|Mag|BodyBody|\\(\\))",
  list(
    "t" = "Time",
    "f" = "Frequency",
    "Acc" = "Accelerometer",
    "Gyro" = "Gyroscope",
    "Mag" = "Magnitude",
    "BodyBody" = "Body",
    "()" = ""
  ),
  measurements
)


# Load Training Data
## read in train, filtering based on features needed, using with=False to retain data.frame class
train <- fread(file.path(path, "/UCI HAR Dataset/train/X_train.txt"))[, featuresNeeded, with = FALSE]
setnames(train, colnames(train), measurements) # change column name based on measurement

activityTrain <-
  fread(file.path(path, "/UCI HAR Dataset/train/y_train.txt"),
        col.names = "Activity")
subjectTrain <-
  fread(file.path(path, "/UCI HAR Dataset/train/subject_train.txt"),
        col.names = "SubjectNo.")

train <- cbind(activityTrain, subjectTrain, train) # bind all columns together

# Load Test Data
test <- fread(file.path(path, "/UCI HAR Dataset/test/X_test.txt"))[, featuresNeeded, with = FALSE]
setnames(test, colnames(test), measurements)

activityTest <-
  fread(file.path(path, "/UCI HAR Dataset/test/y_test.txt"),
        col.names = "Activity")
subjectTest <-
  fread(file.path(path, "/UCI HAR Dataset/test/subject_test.txt"),
        col.names = "SubjectNo.")

test <- cbind(activityTest, subjectTest, test) 

# Important step merging the test and training data by rows
testTrain <- rbind(train, test)

# factor Activity column based on activity labels
# use factor() to set own levels and labels
testTrain[["Activity"]] <- factor(testTrain[, Activity]
                                  , levels = activityLabels[["classLabels"]]
                                  , labels = activityLabels[["activityNames"]]
)

# as.factor() to create turn subject numbers into factors
testTrain[["SubjectNo."]] <- as.factor(testTrain[, SubjectNo.])

# melt then cast the data table
testTrain <- melt.data.table(testTrain, id=c("SubjectNo.", "Activity")) # melt down to variable & value
testTrain <- dcast(testTrain, SubjectNo. + Activity ~ variable, mean) # average of SubjectNo & Activity

# write final tidy data into new file
fwrite(testTrain, file="TidyDataFile.txt")
