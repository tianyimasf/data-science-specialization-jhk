
## This function returns the hospital at a specific rank at every
## state given an outcome
## The higher the mortality rate the hospital has, the lower their 
## rank is (higher numerically).

rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  outcome_data <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that outcome is valid
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank
  findhospital <- function(hospitals, outcome, num = "best") {
    
    ## Getting index of outcome column in outcome_data
    index <- 0
    if (outcome == "heart attack") index <- 11
    if (outcome == "heart failure") index <- 17
    if (outcome == "pneumonia") index <- 23  
    
    ## Order the hospital name column by the outcome column
    ## Preparing data
    hospitals[,index] <- as.numeric(hospitals[,index])
    hospitals <- hospitals[!is.na(hospitals[, index]), ]
    
    ## Get order and reorder
    order <- order(hospitals[,index], hospitals$Hospital.Name) ## Use name to break ties
    ordered_outcome <- hospitals[order, ]
    
    ## Pick out the rank
    hospital <- ordered_outcome[as.numeric(num), 2]
    if (num == "best") hospital <- ordered_outcome[1, 2]
    if (num == "worst") hospital <- ordered_outcome[nrow(ordered_outcome), 2]
    
    hospital
  }
  
  outcome_data <- outcome_data[!is.na(outcome_data$State), ]
  
  info <- mapply(findhospital, split(outcome_data, outcome_data$State), outcome, num)
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  c(hospital = info, state = names(info))
}
