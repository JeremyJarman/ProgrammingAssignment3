## Jeremy Jarman
## R programming module week 4 
## Assignment 3

## this function returns the name of the hospital with the lowest 30 Day mortality rate
## of a given clinical outcome for any given state

best <- function (state,outcome){
        
        ##reads in the hospital outcome data 
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## produce a new data frame with just the columns of interest and names the columns
        comp <- as.data.frame(cbind(data[,2],data[,7],data[,11],data[,17],data[,23]), stringsAsFactors = FALSE)
        colnames(comp) <- c("HospitalName","State","Heart Attack Mortality","Heart Failure Mortality","Pneumonia Mortality")
        
        ## Set up a list of states and outcomes to compare the input to
        states <- unique(data[,7], incomparables = FALSE)
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        ## Established an index which is related to the outcome input
        index<- NULL
        if (outcome == "heart attack"){index <-3}
        else if (outcome == "heart failure"){index <- 4}
        else {index <- 5}
        
        ## Test if state and outcome are valid
        if (!state %in% states){stop ("invalid state")}
        else if (!outcome %in% outcomes){stop ("invalid outcome")}
        
        ## Subset to include only the required state, remove NAs and order by outcome and hospital name   
        else {
                
                stateonly <- subset(comp, State == state)
                suppressWarnings(stateonly[,index]<-as.numeric(stateonly[,index]))
                cleandata <- na.omit(stateonly)
                result <- cleandata[order(cleandata[,index],cleandata[,1]),]
        
        }
       ## Returns the name of the hospital at the top of the list
        return (result[1,1])
      
        
}