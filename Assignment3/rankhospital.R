## Get the ranks of hospital in a state
## This function accept three arguments
## 1. The two character state code
## 2. The outcome value like heart attack
## 3. The rank of hospital to get retrieved.
## The default value of rank will be "best".
## And the rank accept values like "best",1,2,..,"worst"
## Return the name of hospital satisfying the rank

rankhospital <- function(state, outcome, num="best"){
    ## Read column data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    ## Check that state value is valid
    if(isValidState(data,state)==FALSE){
        stop("invalid state")
    }
    ## Check that outcome is valid
    if(isValidOutcome(outcome)==FALSE){
        stop("invalid outcome")
    }
    ## Return the hospital name in the state with the given rank
    ## for 30-day death rate
    stateData <- subset(data, data$State == state)
    index <- getInterestedColumnIndex(outcome)
    suppressWarnings(stateData[,index] <- as.numeric(stateData[,index]))
    sortedOrder <- order(stateData[,index],stateData[,2])
    sortedStateData <- stateData[sortedOrder,]
    rcnt <- nrow(sortedStateData)
    result <- NULL
    if(rcnt==0){
        result <- NA
    }else if(num=="best"){
        result <- sortedStateData[1,]$Hospital.Name
    }else if(num=="worst"){
        result <- sortedStateData[rcnt,]$Hospital.Name
    }else if(num>rcnt){
        result <- NA
    }else{
        result <- sortedStateData[num,]$Hospital.Name
    }
    result
}

## Check whether an input state code is valid or not
isValidState = function(data, state){
    asubset <- subset(data, data$State == state)
    isValid <- TRUE
    if(nrow(asubset) == 0){
        isValid <- FALSE
    }
    isValid
}

## Check whether an input outcome is valid or not
isValidOutcome <- function(outcome){
    allOutcomes <- c("heart attack","heart failure","pneumonia")
    isValid <- TRUE
    if(!any(allOutcomes==outcome)){
        isValid <- FALSE
    }
    isValid
}

## Get the index of column of interested outcomes from data frame
getInterestedColumnIndex <- function(outcome){
    index <- 0
    if(outcome=="heart attack"){
        index <- 11
    } else if (outcome=="heart failure"){
        index <- 17
    } else if(outcome=="pneumonia"){
        index <- 23
    }
    index
}
