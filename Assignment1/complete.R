complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    vLen <- length(id)
    resultData <- matrix(nrow=vLen, ncol=2)
    colnames(resultData) <- c("id","nobs")
    for(index in 1:vLen){
        data <- getFileData(directory,id[index])
        dSub <- subset(data,!is.na(sulfate) & !is.na(nitrate))
        #print(dSub)
        resultData[index,1] <- id[index]
        resultData[index,2] <- nrow(dSub)
    }
    resFrame <- data.frame(resultData)
    resFrame
}
getFileData <- function(directory,the_id){
  filePath <- getFilePath(directory,the_id)
  #print(filePath)
  fdata <- read.csv(file=filePath,head=TRUE,sep=',')
  fdata
}
getFilePath <- function(directory,theId){
  fileNamePrefix <- ""
  if(theId<10){
    fileNamePrefix <- "00"
  }else if(theId>9 & theId<100){
    fileNamePrefix <- "0"
  }
  filePath <- paste(c(directory,"/",fileNamePrefix,theId,".csv"),collapse='')
  filePath
}
