
## Plot the 30-day mortality rates for heart attack

outcome <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

