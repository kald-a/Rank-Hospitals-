rankall <- function(outcome, num = 'best'){
  library(dplyr)
  ## Read the data and assign it to outcomea
  outcomea <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  ## Take the needed data and bind it to new data frame
  outcomedata <- data.frame(cbind(outcomea$Hospital.Name,outcomea$State,outcomea$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcomea$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcomea$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  ## Chenge the names of the columons.
  names(outcomedata) <- c("hospital name", "state", "heart attack", "heart failure", "pneumonia")
  ## Check if the state and the outcome valid
  if (!(outcome %in% names(outcomedata))){
    stop('invalid outcome')
  }
  # assign outcomedata to x
  x <- outcomedata
  # create empty data frame
  h <- data.frame()
  # loop and take state by state 
  for (i in unique(x$state)){
    # filter by the state
    y <- filter(x, state == i)
    # order the data by outcome from low to high and order hospital name alphabetically
    m <- arrange(y, as.numeric(as.character(y[,outcome])), y[,'hospital name'])
    # check and get the result 
    if (num %in% 1:nrow(m)){
      d <-as.character(m[num,1])
      h <- rbind(h, data.frame(d,i))
    }
    if (!(num %in% 1:nrow(m))){
      if (num == 'best'){
        d <-as.character(m[1,1])
        h <- rbind(h, data.frame(d,i))
      }
      if (num == 'worst'){
        m <- arrange(y, desc(as.numeric(as.character(y[,outcome]))), y[,'hospital name'])
        d <- as.character(m[1,1])
        h <- rbind(h, data.frame(d,i))
      }
      if (num != 'best' & num != 'worst'){
        d <- '<NA>'
        h <- rbind(h, data.frame(d,i))
      }
    }
  }
  names(h) <- c('hospital name', 'state')
  v <- arrange(h,as.character(h[,'state']))
  return(v)
}



