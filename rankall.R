#rankall function. ranks all outcome var in given states, and returns the 
#winner of each state for given rank


rankall <- function(outcome, num = "best") {
        #Initiation
        outcome_vector <- c(11, 17, 23)
        outcome_names <- c("heart attack", "heart failure", "pneumonia")
        
        #Read the outcome data
        my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        outcome_check <- sapply(outcome_names, function(x) x == outcome) 
        #logical vector of the entered outcome choice
        
        if(any(outcome_check) != TRUE) { #if outcome name is false stop
                stop("invalid outcome")
                geterrmessage()
        } else { #if both inputs are correct, initiate the outcome col number
                real_outcome <- outcome_vector[outcome_check]
        }
        
        my_data[, real_outcome] <- as.numeric(my_data[, real_outcome])
        #transform into numeric for comparison, NAs introduced by coercion
        #only the datas of the selected state
        clear_data <- my_data[!is.na(my_data[, real_outcome]),]
        #clear NAs
        
        col_nums <- c(2, 0, 7)
        col_nums[2] <- real_outcome
        #necessary columns vector
        
        tidy_data <- clear_data[, col_nums]
        #leave unnecessary columns
        #data made of hospital name, state, and outcome value
        colnames(tidy_data) <- c("Hospital", "Outcome", "State")
        states_data <- split(tidy_data, tidy_data[,3])
        #split the data into a list by the state names
        all_states <- unique(tidy_data[,3])
        #vector of different states present
        res <- data.frame(nrow = length(all_states), ncol = 2)
        #initiation of results matrix. 
        # nrow <- number of states
        # ncol <- the result variables
        colnames(res) <- c("hospital", "state")
        #name the columns of the results matrix
        
        if(num == "best") { #if the best rate is required
                
                for(i in seq_along(all_states)){
                        
                        state_data <- states_data[[i]]
                        #get the matrix of the state
                        
                        rank_vect <- order(state_data[, 2], state_data[,1])
                        #rank vector depending on the outcome, ties broken with 
                        #hospital name alphabetically
                        
                        ordered_data<- state_data[rank_vect, ]
                        #order data according to the outcome var
                        
                        res[i, 1] <- ordered_data[1, 1]
                        res[i, 2] <- ordered_data[1, 3]
                        #append the results matrix
                        
                        
                }
                
                return(res)        
                
                
        } else if(num == "worst"){ # if the worst rate is required
                
                for(i in seq_along(all_states)){
                        
                        state_data <- states_data[[i]]
                        #get the matrix of the state
                        
                        rank_vect <- order(state_data[, 2], state_data[,1])
                        #rank vector depending on the outcome, ties broken with 
                        #hospital name alphabetically
                        
                        ordered_data<- state_data[rank_vect, ]
                        #order data according to the outcome var
                        
                        res[i, 1] <- ordered_data[nrow(ordered_data), 1]
                        res[i, 2] <- ordered_data[nrow(ordered_data), 3]
                        #append the results matrix
                        
                        
                }
                
                return(res)  
                
        } else { # if a spesific rank is required
                
                for(i in seq_along(all_states)){
                        
                        state_data <- states_data[[i]]
                        #get the matrix of the state
                        
                        rank_vect <- order(state_data[, 2], state_data[,1])
                        #rank vector depending on the outcome, ties broken with 
                        #hospital name alphabetically
                        
                        ordered_data<- state_data[rank_vect, ]
                        #order data according to the outcome var
                        
                        res[i, 1] <- ordered_data[num, 1]
                        res[i, 2] <- ordered_data[num, 3]
                        #append the results matrix
                        
                        
                }
                
                return(res)  
                
        }
        
}

