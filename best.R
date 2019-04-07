
#a function to get minimum value of desired outcome in a given state

best <- function(state, outcome) {
        #Initiation
        outcome_vector <- c(11, 17, 23)
        outcome_names <- c("heart attack", "heart failure", "pneumonia")
        
        #Read outcome data
        my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #Check that state and outcome are valid
        state_check <- sapply(my_data$State, function(x) x == state)
        #logical vector of the entered state
        outcome_check <- sapply(outcome_names, function(x) x == outcome) 
        #logical vector of the entered outcome choice
        if(any(state_check) != TRUE){ #if state name is false give stop 
                stop("invalid state")
                geterrmessage()
        } else if(any(outcome_check) != TRUE) { #if outcome name is false stop
                stop("invalid outcome")
                geterrmessage()
        } else { #if both inputs are correct, initiate the outcome col number
                real_outcome <- outcome_vector[outcome_check]
        }
        
        #Return hospital name in that state with lowest 30 day death rate
        state_data <- subset(my_data, State %in% state)
        
        state_data[, real_outcome] <- as.numeric(state_data[, real_outcome])
        #transform into numeric for comparison, NAs introduced by coercion
        #only the datas of the selected state
        
        min_value <- min(state_data[, real_outcome], na.rm = TRUE)
        #the min value of required outcome
        
        my_hosp_vect <- sapply(state_data[, real_outcome], function(x) x == min_value)
        #which hospitals have the min value logical
        
        my_hosps <- state_data[my_hosp_vect, ] 
        #all hospitals with the min value
        
        my_hosp <- min(my_hosps[,2], na.rm = TRUE)
        return(my_hosp)
        
}

