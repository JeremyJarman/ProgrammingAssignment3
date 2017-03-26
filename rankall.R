rankall <- function (outcome, num = "best"){
        
        ##reads in the hospital outcome data and defines an output variable 
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        output <- NULL
        
        ## produce a new data frame with just the columns of interest and names the columns
        comp <- as.data.frame(cbind(data[,2],data[,7],data[,11],data[,17],data[,23]), stringsAsFactors = FALSE)
        colnames(comp) <- c("hospital",
                            "state",
                            "Heart Attack Mortality",
                            "Heart Failure Mortality",
                            "Pneumonia Mortality")
        
        ## Set up a list of states and outcomes to compare the input to
        states <- sort(unique(data[,7], incomparables = FALSE))

        
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        ## Established an index which is related to the outcome input
        index<- NULL
        if (outcome == "heart attack"){index <-3}
        else if (outcome == "heart failure"){index <- 4}
        else {index <- 5}
        
        ## Test if state and outcome are valid
        
        if (!outcome %in% outcomes){stop ("invalid outcome")}
        
        ## this loop generates the required ranked hospital for each state
        ## and appends it to the output data frame
        else {
                
                i <- 1
                output <- NULL
                
                while (i <= length(states)){
                        stateonly <- subset(comp, state == states[i])
                        suppressWarnings(stateonly[,index]<-as.numeric(stateonly[,index]))
                        cleandata <- na.omit(stateonly)
                        result <- cleandata[order(cleandata[,index],cleandata[,1]),]     
                        i = i + 1
                        
                        if (is.numeric(num)){
                        output <- rbind(output,result [num,1:2])
                        }
                        
                        else if (num == "best"){
                        output <- rbind(output,result [1,1:2])
                        }
                        
                        else if (num == "worst"){
                        output <- rbind(output,result[nrow(result),1:2])
                        }
                
                        else {output = NA}
                }
                
                rownames(output) <- states
                return (output)
                
                }
}