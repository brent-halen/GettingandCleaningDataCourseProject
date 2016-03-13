packages <- c("data.table", "reshape2", "knitr", "markdown", "qtl")
sapply(packages, require, character.only=TRUE, quietly=FALSE)
path <- getwd()
path

FileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "Dataset.zip"
if(!file.exists(path)) {dir.create(path)}
download.file(FileUrl, file.path(path,file))

executable <- file.path("C:", "Program Files", "7-Zip", "7z.exe")
parameters <- "x"
command <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path,file), "\""))
system(command)

pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

datatableSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
datatableSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))

datatableActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
datatableActivityTest <- fread(file.path(pathIn, "test", "Y_test.txt"))

fileToDataTable <- function (file) {
    df <- read.table(file)
    datatable <- data.table(df)
    
}
datatableTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
datatableTest <- fileToDataTable(file.path(pathIn , "test" , "X_test.txt"))

datatableSubject <- rbind(datatableSubjectTrain, datatableSubjectTest)
setnames(datatableSubject, "V1", "subject")
datatableActivity <- rbind(datatableActivityTrain, datatableActivityTest)
setnames(datatableActivity, "V1", "activityNum")
datatable <- rbind(datatableTrain, datatableTest)

datatableSubject <- cbind(datatableSubject, datatableActivity)
datatable <- cbind(datatableSubject, datatable)

setkey(datatable, subject, activityNum)

datatableFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(datatableFeatures, names(datatableFeatures), c("featureNum", "featureName"))

datatableFeatures <- datatableFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

datatableFeatures$featureCode <- datatableFeatures[, paste0("V", featureNum)]
head(datatableFeatures)
datatableFeatures$featureCode

select <- c(key(datatable), datatableFeatures$featureCode)
datatable <- datatable[, select, with=FALSE]

datatableActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(datatableActivityNames, names(datatableActivityNames), c("activityNum", "activityName"))

datatable <- merge(datatable, datatableActivityNames, by="activityNum", all.x=TRUE)

setkey(datatable, subject, activityNum, activityName)

datatable <- data.table(melt(datatable, key(datatable), variable.name="featureCode"))

datatable <- merge(datatable, datatableFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

datatable$activity <- factor(datatable$activityName)
datatable$feature <- factor(datatable$featureName)

grepthis <- function (regex){
    grepl(regex, datatable$feature)
}

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

datatable$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
datatable$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
datatable$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

r1 <- nrow(datatable[, .N, by=c("feature")])
r2 <- nrow(datatable[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

setkey(datatable, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
datatableTidy <- datatable[, list(count = .N, average = mean(value)), by=key(datatable)]

##if(!file.exists("makeCodebook.Rmd")) {file.create("makeCodebook.Rmd")}
##knit("makeCodebook.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=FALSE)
##markdownToHTML("codebook.md", "codebook.html")