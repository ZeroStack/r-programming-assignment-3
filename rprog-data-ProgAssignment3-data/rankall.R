rankall <- function(outcome, num) {
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        #load outcomes into data variable
        
        all_states <- unique(data[7])
        #unique states in data
        
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        #valid outcomes
        valid_numbers <- c(11,17,23)
        #valid numbers associated with outcomes
        
        valid_table <- data.frame(valid_outcomes, valid_numbers)
        #merging valid outcomes and associated numbers
        
        
        #VALIDTY CHECKERS
        any(valid_outcomes==outcome)
        #Returns TRUE if outcome is valid
        valid_num <- is.numeric(num)
        #checks of num is valid
        
        
        if(any(valid_outcomes==outcome)) {
                
                
                data_columnid <- valid_table[valid_table[]==outcome,2]
                #identifies appropriate column id
                
                data_column <- data[,c(1,2,7,data_columnid)]
                #data by state and outcome
                
                data_column[,4] <- as.numeric(data_column[,4])
                
                states_data <- split(data_column, data_column[3])
                
                #clean_datacolumn <- data_column[complete.cases(data_column),]
                
                #rank_data <- clean_datacolumn[order(clean_datacolumn[4]),]
                #rank_data[,5] <- c(1:nrow(rank_data))
                
                
                #hospital_name <- rank_data[rank_data[5]==num][2]
                #hospital name by rank
                
                #rank_outcome <- as.numeric(rank_data[rank_data[5]==num][4])
                #the outcome of the rank
                
                
                
                if(valid_num) {
                        
                        colClasses <- c("character", "character")
                        col.names <- c("hospital", "state")
                        
                        y <- read.table(text="", colClasses = colClasses, col.names = col.names)
                        
                        
                        n = 1
                        while(n <= length(states_data)) {
                                
                                
                                
                                #state_data[,4] <- as.numeric(state_data[,4])
                                
                                state_data <- states_data[n]#[order(states_data[n][4])]
                                
                                state_data1 <- states_data[1]#[order(states_data[1][4])]
                                
                                state_data <- as.data.frame(state_data)
                                state_data[,4] <- as.numeric(state_data[,4])
                                
                                state_data <- state_data[order(state_data[4]),]
                                
                                #state_data <- as.data.frame(state_data)
                                state_data[,5] <- c(1:nrow(state_data))
                                
                                #state_matrix <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 2))
                                
                                #state_matrix[1,2] <- state_data[state_data[5]==num,][3][1,1]
                                
                                #z <- state_data[state_data[5]==num,][2][1,1]
                                #state_matrix[2,1] <- state_data[state_data[5]==num,][3]
                                y[n,1] <- state_data[state_data[5]==num,][2][1,1]
                                y[n,2] <- state_data[state_data[5]==num,][3][1,1]
                                #rownames(y)[n] <- state_data[state_data[5]==num,][3][1,1]
                                
                                
                                n <- n + 1
                                
                        }
                            
                }
                else if(num=="best") {
                        
                        
                }
                
                else if(num=="worst"){
                        
                        colClasses <- c("character", "character")
                        col.names <- c("hospital", "state")
                        
                        y <- read.table(text="", colClasses = colClasses, col.names = col.names)
                        
                        
                        n = 1
                        while(n <= length(states_data)) {
                                
                                
                                
                                
                                
                                #state_data[,4] <- as.numeric(state_data[,4])
                                
                                state_data <- states_data[n]#[order(states_data[n][4])]
                                
                                
                                
                                
                                
                                state_data <- as.data.frame(state_data)
                                state_data[,4] <- as.numeric(state_data[,4])
                                
                                state_data <- state_data[order(state_data[4]),]
                                
                                state_data1 <- state_data[order(state_data[4]),]
                                
                                
                                #state_data <- as.data.frame(state_data)
                                state_data[,5] <- c(1:nrow(state_data))
                                
                                num <- ncol(state_data)
                                
                                #state_matrix <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 2))
                                
                                #state_matrix[1,2] <- state_data[state_data[5]==num,][3][1,1]
                                
                                #z <- state_data[state_data[5]==num,][2][1,1]
                                #state_matrix[2,1] <- state_data[state_data[5]==num,][3]
                                y[n,1] <- state_data[state_data[5]==num,][2][1,1]
                                y[n,2] <- state_data[state_data[5]==num,][3][1,1]
                                #rownames(y)[n] <- state_data[state_data[5]==num,][3][1,1]
                                
                                
                                n <- n + 1
                                
                        }
                }
        }
        
        else if(!any(valid_outcomes==outcome)) {
                stop("Error in rankall(",'"', state, '"', ",", " ", '"', outcome, '") : invalid outcome')
        }
        
        

        
        rownames(y) <- names(states_data)
        
        y[2] <- names(states_data)
        
        
        y
        
        
        
        
        
        
}

