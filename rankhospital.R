rankhospital <- function(state, outcome, num = 'best'){
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
  ## check if num in the data
  if (num %in% 1:nrow(m)){
    d <- m[num,1]
  }
  ## check if num best or worst
  if (!(num %in% 1:nrow(m))){
    if (num == 'best'){
      d <- m[1,1]
    }
    if (num == 'worst'){
      m <- arrange(y, desc(as.numeric(as.character(y[,outcome]))), y[,'hospital name'])
      d <- m[1,1]
    }
    if (num != 'best' & num != 'worst'){
      stop('invalid input')
    }
  }
  return(as.character(d))
}

