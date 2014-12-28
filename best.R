best <- function(state, outcome) {
  
  ## Read outcome data
  hosp.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Store names of state and outcome in a array
  st_tbl <- unique(hosp.df$State)
  outcome_name <- c("heart attack", "heart failure",  "pneumonia")
  
  ## Check invalid state
  if(!any (st_tbl==state)) 
    stop("Invalid state")  
  
  ## Check invalid outcome
  if(!any (outcome_name == outcome)) 
    stop("Invalid outcome")
  
  #Get subset by state
  sub.hosp.df <- subset(hosp.df[,c(2,7,11,17,23)],hosp.df$State == state)
  
  ## Rename name of variables 
  names(sub.hosp.df)[1] <- "Hosp_Name"
  names(sub.hosp.df)[2] <- "State"
  names(sub.hosp.df)[3] <- "heart attack"
  names(sub.hosp.df)[4] <- "heart failure"
  names(sub.hosp.df)[5] <- "pneumonia"
  
  # Convert mortality data rate to numeric
  sub.hosp.df[,3] == as.numeric(sub.hosp.df[,3]) # Heart Attack
  sub.hosp.df[,4] == as.numeric(sub.hosp.df[,4]) # Heart Failure
  sub.hosp.df[,5] == as.numeric(sub.hosp.df[,5]) # Pneumonia
  
  
  ## Grab only rows with our state value  
  sub.hosp.df <- sub.hosp.df[sub.hosp.df$State==state & sub.hosp.df[outcome] != 'Not Available', ]
  
  ## Create array of values
  vals <- sub.hosp.df[, outcome]
  
  # which.min - new function learnt
  # gives the index of values
  rowNum <- which.min(vals)
  
  ## Return hospital name in that state with lowest 30-day death rate
  sub.hosp.df[rowNum, ]$Hosp_Name
}
