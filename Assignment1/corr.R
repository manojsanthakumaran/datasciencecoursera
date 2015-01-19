corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
  
    ## Return a numeric vector of correlations

    dfComplete <- complete(directory)
    dfThreshold <- subset(dfComplete,nobs>threshold)
    matchIds <- dfThreshold$id
    matchIds <- sort(matchIds)
    idLen <- length(matchIds)
    xVector <- vector(mode="numeric", length=0)
    yVector <- vector(mode="numeric", length=0)
    resultData <- vector(mode="numeric", length=0)
    if(idLen>0){
        for(index in 1:idLen){
            data <- getFileData(directory,matchIds[index])
            xVector <- append(xVector,data$sulfate)
            yVector <- append(yVector,data$nitrate)
            resultData <- append(resultData,round(cor(yVector,xVector,use="complete.obs"),digits=5))
        }
    }
    resultData
}
