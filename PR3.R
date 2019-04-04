# (http://hospitalcompare.hhs.gov)

# focus on the variables for Number 19
# "Outcome of Care Measures.csv"
# and Number 11 "Hospital_Data.csv"
# print out the pages 19 and 11

# the numbers of the variables for each table
# indicate column indices in each table.
# i.e. "Hospital Name" is column 2 in the 
# outcome-of-care-measures.csv file

# Plot the 30 day mortality rates for heart attack

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11])