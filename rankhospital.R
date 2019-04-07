#ranks hospitals of given state of given outcome. returns the wanted ranking
#hospital's name


rankhospital <- function(state, outcome, num = "best") {
        #Initiation
        outcome_vector <- c(11, 17, 23)
        outcome_names <- c("heart attack", "heart failure", "pneumonia")
        
        #Read the outcome data
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
        
        #Return hospital name in given state with the given rank of 30day death
        #rate
        # If the number given by num is larger than the number of hospitals
        #in that state, then the function should return NA
        
        state_data <- subset(my_data, State %in% state)
        
        state_data[, real_outcome] <- as.numeric(state_data[, real_outcome])
        #transform into numeric for comparison, NAs introduced by coercion
        #only the datas of the selected state
        clear_data <- state_data[!is.na(state_data[, real_outcome]),]
        #clear NAs
        
        col_nums <- 2
        col_nums[2] <- real_outcome
        #necessary columns vector
        
        tidy_data <- clear_data[, col_nums]
        #leave unnecessary columns
        
        if(num == "best") { # if the best rate is required
                min_value <- min(tidy_data[, 2], na.rm = TRUE)
                #the min value of required outcome
                
                my_hosp_vect <- sapply(tidy_data[, 2], function(x) x == min_value)
                #which hospitals have the min value logical
                
                my_hosps <- tidy_data[my_hosp_vect, ] 
                #all hospitals with the min value
                
                my_hosp <- min(my_hosps[,1], na.rm = TRUE)
                return(my_hosp)
                
        } else if(num == "worst"){ # if the worst rate is required
                max_value <- max(tidy_data[, 2], na.rm = TRUE)
                #the max value of required outcome
                
                my_hosp_vect <- sapply(tidy_data[, 2], function(x) x == max_value)
                #which hospitals have the max value logical
                
                my_hosps <- tidy_data[my_hosp_vect, ] 
                #all hospitals with the max value
                
                my_hosp <- max(my_hosps[,1], na.rm = TRUE)
                return(my_hosp)
                
        } else { # if a spesific rank is required
                if(num > nrow(tidy_data)){
                        return(NA)
                } else {
                        rank_vect <- order(tidy_data[, 2], tidy_data[,1])
                        #rank vector depending on the outcome, ties broken with 
                        #hospital name alphabetically
                        
                        ordered_data<- tidy_data[rank_vect, ]
                        
                        my_hosp <- ordered_data[num, 1]
                        
                        return(my_hosp)
                        
                }
                
        }        
        
        #x[order(x)] for a vector
        
}
