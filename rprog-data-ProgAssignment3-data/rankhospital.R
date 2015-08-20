
rankhospital <- function(state, outcome, num) {
        
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
        
        any(all_states==state)
        #Returns TRUE if state is valid
        
        any(valid_outcomes==outcome)
        #Returns TRUE if outcome is valid
        
        valid_num <- is.numeric(num)
        #checks of num is valid

        if(any(all_states==state) && any(valid_outcomes==outcome)) {
                
                state_data <-data[data[,7]==state,]
                #outcomes data filetered by state
                
                data_columnid <- valid_table[valid_table[]==outcome,2]
                #identifies appropriate column id
                
                data_column <- state_data[,c(1,2,7,data_columnid)]
                #data by state and outcome
                
                data_column[,4] <- as.numeric(data_column[,4])
                clean_datacolumn <- data_column[complete.cases(data_column),]
                
                rank_data <- clean_datacolumn[order(clean_datacolumn[4]),]
                rank_data[,5] <- c(1:nrow(rank_data))
                
                
                hospital_name <- rank_data[rank_data[5]==num][2]
                #hospital name by rank
                
                rank_outcome <- as.numeric(rank_data[rank_data[5]==num][4])
                #the outcome of the rank
                
                
                
                if(valid_num) {
                        
                        #if both outcomes and state is TRUE
                        
                        hospitals_by_rank <- rank_data[rank_data[4]==rank_outcome,]
                        final_hospital <- hospitals_by_rank[order(rownames(hospitals_by_rank)),][1,2]
                        
                        
                        
                }
                else if(num=="best") {
                        final_hospital <- rank_data[rank_data[5]==1][2]
                        
                }
                
                else if(num=="worst"){
                        final_hospital <- rank_data[rank_data[5]==nrow(rank_data)][2]
                        
                        
                        
                }
        }
        
        else if(!any(all_states==state)) {
                stop("Error in rankhospital(",'"', state, '"', ",", " ", '"', outcome, '") : invalid state')
       
        }
        
        
        else if(!any(valid_outcomes==outcome)) {
                stop("Error in rankhospital(",'"', state, '"', ",", " ", '"', outcome, '") : invalid outcome')
        }
        #hospital_name
        #rank_data
        
        #rank_outcome
        
        hospitals_by_rank
        final_hospital
        
}
