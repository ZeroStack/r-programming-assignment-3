
best <- function(state, outcome) {
        
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
        
        if(any(all_states==state) && any(valid_outcomes==outcome)) {
                #if both outcomes and state is TRUE
                
                
                state_data <-data[data[,7]==state,]
                #outcomes data filetered by state
                
                data_columnid <- valid_table[valid_table[]==outcome,2]
                
                
                #identifies appropriate column id
                
                data_column <- state_data[,c(1,2,7,data_columnid)]
                #data by state and outcome
                
                data_column[,4] <- as.numeric(data_column[,4])
                
                data_summary <- table(data_column[,4])
                #summary of outcomes data
                
                lowest_day_outcome <- min(data_column[4], na.rm = TRUE)
                #lowest day value
                
                number_hospitals <- data_summary[names(data_summary)==lowest_day_outcome]
                #number of hospitals with lowest day
                
                if(number_hospitals==1) {
                        #if hospital number is 1
                        
                        clean_datacolumn <- data_column[complete.cases(data_column),]
                        
                        best_hospital <- clean_datacolumn[clean_datacolumn[4]==lowest_day_outcome]
                        best_hospital <- best_hospital[2]
                        best_hospital
                }
                
                else if(number_hospitals > 1) {
                        
                        
                        
                        clean_datacolumn <- data_column[complete.cases(data_column),]
                        
                        best_hospital <- clean_datacolumn[clean_datacolumn[4]==lowest_day_outcome,]
                        
                        best_hospital <- sort(best_hospital$"Hospital.Name")[1]
                        
                        best_hospital
                        
                        
                }
                
                
        }
        
        else if(!any(all_states==state)) {
                stop("Error in best(",'"', state, '"', ",", " ", '"', outcome, '") : invalid state')
                
                
        
                
        }
        
        else if(!any(valid_outcomes==outcome)) {
                message("Error in best(",'"', state, '"', ",", " ", '"', outcome, '") : invalid outcome')
        }
        #number_hospitals
        
        
        
       
       
}
        