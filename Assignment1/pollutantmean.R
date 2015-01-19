pollutantmean <- function(directory,pollutant,id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    colIndx <- getPollutantColIndex(pollutant)
    if(colIndx==-1){
        print("invalid pollutant name !!")
        return()
    }
    
    vLen <- length(id)
    resultData <- vector(mode="numeric", length=0)
    for(index in 1:vLen){
        data <- getFileData(directory,id[index])
        resultData <- append(resultData,data[,colIndx])
    }
    finalMean <- mean(resultData,na.rm=TRUE)
    round(finalMean,digits=3)
}
getPollutantColIndex <- function(col){
    indx <- if("sulfate"==col){
      2
    } else if("nitrate"==col){
      3
    } else {
      -1
    }
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
