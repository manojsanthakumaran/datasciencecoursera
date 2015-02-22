## This function load all the row data from folders
## and make them tidy, by following steps:
## Step 1: Consolidate test and train data
## Step 2: Take mean and standard deviation of consolidated data
## Step 3: Merge all the dataset to one

## Load a partilucar row data file
## and return the data frame
loadDataAsTable <- function(file) {
    data <- read.table(file, sep=" ", header=FALSE, fill=TRUE)
    data
}
## Funtion to consolidate two row data frames
## after finding the mean and SD
getConsolidatedData <- function(dt1,dt2,flg) {
    dt3 <- mergeData(dt1,dt2)
    data <- calcRowMeans(dt3,flg)
    data <- calcRowSD(dt3,data,flg)
    data
}
## Function to merge two data frames
mergeData <- function(dt1,dt2) {
    data <- merge(dt1, dt2, all=TRUE)
    data
}
## Function to calculate row wise mean
calcRowMeans <- function(dt,flg) {
    data <- data.frame(XYZ_Mean=rowMeans(dt,na.rm=TRUE))
    names(data)[1] <- paste0(flg, "_Mean")
    data
}
## Function to calculate row wise SD
calcRowSD <- function(dt,dest,flg) {
    dest <- transform(dest, XYZ_SD=apply(dt, 1, sd, na.rm=TRUE))
    names(dest)[2] <- paste0(flg,"_SD")
    dest
}
## Function to merge two different sized data frames to one
## after filling the missing column values to NA
cbindWithNAFill <- function(dt1,dt2) {
    dt1rows <- nrow(dt1)
    dt2rows <- nrow(dt2)
    start <- 0
    end <- 0
    smalldata <- dt1
    bigdata <- dt2
    if(dt1rows < dt2rows){
        start <- dt1rows
        end <- dt2rows
        smalldata <- dt1
        bigdata <- dt2
    } else {
        start <- dt2rows
        end <- dt1rows
        smalldata <- dt2
        bigdata <- dt1
    }
    numcols <- ncol(smalldata)
    for(i in start:end){
        for(j in 1:numcols){
            smalldata[i,j]=NA
        }
    }
    finaldata <- cbind(smalldata,bigdata)
    finaldata
}

## Load first set of files
data1 <- loadDataAsTable("./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt")

data2 <- loadDataAsTable("./UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt")

## Generate consolidated data output for X value
x_data <- getConsolidatedData(data1,data2,"ACC_X")

data1 <- loadDataAsTable("./UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt")

data2 <- loadDataAsTable("./UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt")

## Generate consolidated data output for Y values
y_data <- getConsolidatedData(data1,data2,"ACC_Y")

data1 <- loadDataAsTable("./UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt")

data2 <- loadDataAsTable("./UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt")

## Generate consolidated data output for Z values
z_data <- getConsolidatedData(data1,data2,"ACC_Z")

## Merge all X,Y and Z data outputs to single one
con_data <- cbindWithNAFill(cbindWithNAFill(x_data,y_data),z_data)

data1 <- loadDataAsTable("./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt")

data2 <- loadDataAsTable("./UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt")

x_data <- getConsolidatedData(data1,data2,"GYRO_X")

data1 <- loadDataAsTable("./UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt")

data2 <- loadDataAsTable("./UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt")

y_data <- getConsolidatedData(data1,data2,"GYRO_Y")

data1 <- loadDataAsTable("./UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt")

data2 <- loadDataAsTable("./UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt")

z_data <- getConsolidatedData(data1,data2,"GYRO_Z")

## Merge all X,Y and Z data outputs to single one
con_data <- cbindWithNAFill(cbindWithNAFill(cbindWithNAFill(con_data,x_data),y_data),z_data)

data1 <- loadDataAsTable("./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt")

data2 <- loadDataAsTable("./UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt")

x_data <- getConsolidatedData(data1,data2,"TOTAL_X")

data1 <- loadDataAsTable("./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt")

data2 <- loadDataAsTable("./UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt")

y_data <- getConsolidatedData(data1,data2,"TOTAL_Y")

data1 <- loadDataAsTable("./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt")

data2 <- loadDataAsTable("./UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt")

z_data <- getConsolidatedData(data1,data2,"TOTAL_Z")

## Merge all X,Y and Z data outputs to single one
con_data <- cbindWithNAFill(cbindWithNAFill(cbindWithNAFill(con_data,x_data),y_data),z_data)

## Write the final output data frame to a file
write.table(con_data,file="final_tidy_data_out.txt",row.name=FALSE)

## Done
