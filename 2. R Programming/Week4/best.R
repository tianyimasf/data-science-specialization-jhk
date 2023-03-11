
## This function returns the best hospital given a valid state and outcome.
## The hospital with the lowest mortality rate is the best hospital.

best <- function(state, outcome) {

  ## Read outcome data
  outcome_data <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!outcome %in% outcomes) stop("invalid outcome")
  if (!state %in% outcome_data$State) stop("invalid state")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Getting all hospitals from the state
  hospitals <- outcome_data[outcome_data$State == state, ]
  
  ## Getting index of outcome column in outcome_data
  index <- 0
  if (outcome == "heart attack") index <- 11
  if (outcome == "heart failure") index <- 17
  if (outcome == "pneumonia") index <- 23
  
  ## Order the hospital name column by the outcome column
  hospitals[,index] <- as.numeric(hospitals[,index])
  order <- order(hospitals[,index], hospitals$Hospital.Name) ## Use name to break ties
  ordered_outcome <- hospitals[order, ]
  
  ordered_outcome[1, 2]
}
