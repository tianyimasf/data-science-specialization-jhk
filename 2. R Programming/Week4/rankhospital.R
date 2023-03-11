
## This function returns the hospital at a specific rank given a 
## valid state and outcome
## The higher the mortality rate the hospital has, the lower their 
## rank is (higher numerically).

rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  outcome_data <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!outcome %in% outcomes) stop("invalid outcome")
  if (!state %in% outcome_data$State) stop("invalid state")
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ## Getting all hospitals from the state
  hospitals <- outcome_data[outcome_data$State == state, ]
  
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
