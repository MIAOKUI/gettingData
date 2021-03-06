Project of Getting and Cleaning Data
========================================================
## Getting data and setting working directory 

##downloading and unzip
If you don't have data on your working directory 
and would like to download for remote server
uncomment following code 

```r
# con <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download.file(url=con,destfile="data.zip")
# unzip("data.zip")
```

## If you provide data yourself than putting the "UCI HAR Dataset" on your working directory
Setting working directory 
## Reading all the data into workspace

Reading all the data and saved into seperated variable

```r
setwd("./UCI HAR Dataset/")
activity_labels <- read.table("activity_labels.txt",header = FALSE,stringsAsFactors = FALSE )
object_train <- read.table("./train/subject_train.txt",header = FALSE,stringsAsFactors = FALSE)
object_test <- read.table("./test//subject_test.txt",header = FALSE,stringsAsFactors = FALSE)
feature <- read.table("features.txt",head = FALSE,stringsAsFactors = FALSE)
train_x <- read.table("./train/X_train.txt",header = FALSE,stringsAsFactors = FALSE)
train_y <- read.table("./train/y_train.txt",header = FALSE,stringsAsFactors = FALSE)
test_x <- read.table("./test/X_test.txt",header = FALSE, stringsAsFactors = FALSE)
test_y <- read.table("./test/y_test.txt",header = FALSE, stringsAsFactors = FALSE)
```

## manipulating the feature name make them meanfull 

```r
feature <- feature[,2] 
feature <- gsub("-",".",feature) # replace "-" into "."
feature <- gsub("\\(\\)","",feature) # remove "()"
```

## rename the test train data set with fearture name


```r
names(test_x) <- feature
names(train_x)<- feature
```

## combine training x  and testing x dataset

```r
total_x <- rbind(train_x, test_x)
object <- c(object_train[,1],object_test[,1])
```

## combine training y and testing y dataset 

```r
label <- rbind(train_y[1],test_y[1])
```

## adding label and object name into varible name


```r
label_merge <- merge(label,activity_labels,ID = "V1")
```

## combine feature,label, object ID into  final total dataset  

```r
dat <- data.frame(object = object,
                   activity.labels = label_merge[,2],
                   total_x)
```

## sorting dataset by object 

```r
dat <- dat[order(dat$object),]
names(dat) <- c("object","activity.labels",feature)
```

## creating a filtering vector by regulaer expression to find 
## varible related mean and stander diviation 

```r
filter <- grep("std$|mean$",names(dat))
## getting mean and std 
dat_mean_sd <- dat[,c(1,2,filter)]
## clean rownames
rownames(dat_mean_sd) <- NULL 
```

## creating and new dataset of average all vailble value by object and activity 

```r
dat_new <- aggregate(dat[names(dat)[3:ncol(dat)]],
                     by = dat[c("object","activity.labels")],
                     FUN = mean)
## sorting the new dataset by object order 
dat_new <- dat_new[order(as.numeric(dat_new$object)),]
## clean rownames 
rownames(dat_new) <- NULL
```

## save new dataset into new_data.txt

```r
write.table(dat_new,"newData.txt",row.names = FALSE)
```
