## Get the ranks of hospital in all states
## This function accept two arguments
## 1. The outcome value like heart attack
## 2. The rank of hospital to get retrieved.
## The default value of rank will be "best".
## And the rank accept values like "best",1,2,..,"worst"
## Return a data frame with two columns, hospital name and state code

rankall <- function(outcome, num="best"){
    ## Read column data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    ## Check that outcome is valid
    if(isValidOutcome(outcome)==FALSE){
        stop("invalid outcome")
    }
    ## Return the hospital name and state code in all state with 
    ## the given rank for 30-day death rate
    uno_states <- unique(data$State)
    stateorderindex <- order(uno_states)
    states <- uno_states[stateorderindex]
    index <- length(states)
    x_data <- character(index)
    y_data <- character(index)
    row_names <- character(index)
    index <- 1
    for(st in states){
        a_result <- rankhospital(data,st,outcome,num)
        x_data[index] <- a_result
        y_data[index] <- st
        row_names[index] <- st
        index <- index+1
    }
    result <- data.frame(x_data, y_data)
    names(result) <- c("hospital","state")
    row.names(result) <- row_names
    result
}

## Get the specified rank of a single state
rankhospital <- function(data, state, outcome, num="best"){
    stateData <- subset(data, data$State == state)
    index <- getInterestedColumnIndex(outcome)
    suppressWarnings(stateData[,index] <- as.numeric(stateData[,index]))
    sortedOrder <- order(stateData[,index],stateData[,2])
    sortedStateData <- stateData[sortedOrder,]
    rcnt <- nrow(sortedStateData)
    result <- NULL
    if(rcnt==0){
        result <- "<NA>"
    }else if(num=="best"){
        result <- sortedStateData[1,]$Hospital.Name
    }else if(num=="worst"){
        result <- sortedStateData[rcnt,]$Hospital.Name
    }else if(num>rcnt){
        result <- "<NA>"
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
