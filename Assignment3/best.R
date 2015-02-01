## Finding the best hospital in a state
## This function accept two arguments:
## 1. Two character code of the state
## 2. A outcome, like "heat attack"
## The response will be a character vector with the name of the hospital
##   that has the best (lowest) mortality rate for the specified outcome.

best <- function(state, outcome){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    ## Check that state is valid
    if(isValidState(data,state)==FALSE){
        stop("invalid state")
    }
    ## Check that outcome is valid
    if(isValidOutcome(outcome)==FALSE){
        stop("invalid outcome")
    }
    ## Returns hospital name in that state with the given rank
    ## 30-day death rate
    stateData <- subset(data, data$State == state)
    index <- getInterestedColumnIndex(outcome)
    suppressWarnings(stateData[,index] <- as.numeric(stateData[,index]))
    suppressWarnings(stateData[which.min(stateData[,index]),2])
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
