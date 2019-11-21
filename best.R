best <- function(state, outcome){
  library(dplyr)
  costate <- state
  ## Read the data and asingh it to outcom
  outcomea <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  ## Take the neesded data and bind it to new data frame
  outcomedata <- data.frame(cbind(outcomea$Hospital.Name,outcomea$State,outcomea$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcomea$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcomea$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  ## Chenge the names of the columons.
  names(outcomedata) <- c("hospital name", "state", "heart attack", "heart failure", "pneumonia")
  ## Check if the state and the outcome valid
  if (!(costate %in% outcomedata$state)){
    stop('invalid state')
  }
  if (!(outcome %in% names(outcomedata))){
    stop('invalid outcome')
  }
  ## filter data by the state
  x <- filter(outcomedata, state == costate)
  ## Clean the data remove na
  y <- na.omit(x)
  ## order data by outcome
  m <- arrange(y, as.numeric(as.character(y[,outcome])), y[,'hospital name'])
  ## return the first hospital
  d <- m[1,1]
  return(as.character(d))
}

##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  