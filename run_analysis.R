## Set required packages to a vector.
packages <- c("data.table", "reshape2", "knitr", "markdown", "qtl")
## Use sapply to apply the "require" function to each member of the vector. 
sapply(packages, require, character.only=TRUE, quietly=FALSE)
## Set the current working directory as the "path" variable
path <- getwd()
path

## Set 'FileUrl' to the download location
FileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
## Set the filename of the download
file <- "Dataset.zip"
## Create the directory if it doesn't exist.
if(!file.exists(path)) {dir.create(path)}
## Download the file from the path defined earlier to the filename defined earlier.
download.file(FileUrl, file.path(path,file))

## Set 7-zip executable to unzip the file.
executable <- file.path("C:", "Program Files", "7-Zip", "7z.exe")
parameters <- "x"
## Unzip the file.
command <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path,file), "\""))
system(command)
## Unzip to the 'UCI HAR Dataset' folder.
pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)
## Read Subject Training data and Test data. 
datatableSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
datatableSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))
## Read Activity Training data and Test data.
datatableActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
datatableActivityTest <- fread(file.path(pathIn, "test", "Y_test.txt"))
## Define 'fileToDataTable' function to convert tables read from files to data.table format in R.
fileToDataTable <- function (file) {
    df <- read.table(file)
    datatable <- data.table(df)
    
}
## Convert the data frames to data tables.
datatableTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
datatableTest <- fileToDataTable(file.path(pathIn , "test" , "X_test.txt"))
## Concatenate the data tables.
datatableSubject <- rbind(datatableSubjectTrain, datatableSubjectTest)
setnames(datatableSubject, "V1", "subject")
datatableActivity <- rbind(datatableActivityTrain, datatableActivityTest)
setnames(datatableActivity, "V1", "activityNum")
datatable <- rbind(datatableTrain, datatableTest)
## Merge the columns
datatableSubject <- cbind(datatableSubject, datatableActivity)
datatable <- cbind(datatableSubject, datatable)
## Set Key.
setkey(datatable, subject, activityNum)
## Read the features.txt file to determine which variables in datatable are measurements for the mean & stdev.
datatableFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(datatableFeatures, names(datatableFeatures), c("featureNum", "featureName"))
## Subset the measurements for the mean and stdev. 
datatableFeatures <- datatableFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
## Convert the column numbers to a vector of variable names, which match columns in datatable.
datatableFeatures$featureCode <- datatableFeatures[, paste0("V", featureNum)]
head(datatableFeatures)
datatableFeatures$featureCode
## Subset these variables using the vector of variable names created in the last step. 
select <- c(key(datatable), datatableFeatures$featureCode)
datatable <- datatable[, select, with=FALSE]
## Read activity_labels.txt to add descriptive names to the activities.
datatableActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(datatableActivityNames, names(datatableActivityNames), c("activityNum", "activityName"))
## Merge activity labels.
datatable <- merge(datatable, datatableActivityNames, by="activityNum", all.x=TRUE)
## Add 'activityName' as a key.
setkey(datatable, subject, activityNum, activityName)
## Melt the data table to reshape it from a short and wide format to a tall and narrow format. 
datatable <- data.table(melt(datatable, key(datatable), variable.name="featureCode"))
## Merge activity name. 
datatable <- merge(datatable, datatableFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
## Create new 'activity' and 'feature' variables equivalent to 'activityName' and 'featureName' as a factor classes, respectively. 
datatable$activity <- factor(datatable$activityName)
datatable$feature <- factor(datatable$featureName)
## Separate features from featureName.
grepthis <- function (regex){
    grepl(regex, datatable$feature)
}
## Features with 2 categories.
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
datatable$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
datatable$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
datatable$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
datatable$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category.
datatable$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
datatable$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories.
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
datatable$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
## Check to make sure all possible combinations of 'feature are accounted for by all possible combinations of the factor class variables. 
r1 <- nrow(datatable[, .N, by=c("feature")])
r2 <- nrow(datatable[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2
# Create a data set with the average of each variable for each activity and each subject.
setkey(datatable, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
datatableTidy <- datatable[, list(count = .N, average = mean(value)), by=key(datatable)]
##Following code moved to makeCodebook.Rmd
## Write the dataset to a new file called "Tidydataset.txt".
#write.table(datatableTidy, file = "Tidydataset.txt", row.names=FALSE)

## Make codebook. 

knit("makeCodebook.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=FALSE)
markdownToHTML("codebook.md", "codebook.html")