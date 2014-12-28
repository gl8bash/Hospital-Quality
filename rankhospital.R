rankhospital <- function(state, outcome, num = "best" ) {
  
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
  
  "  Multiline Comment 
  # Convert mortality data rate to numeric
  sub.hosp.df[,3] == as.numeric(sub.hosp.df[,3]) # Heart Attack
  sub.hosp.df[,4] == as.numeric(sub.hosp.df[,4]) # Heart Failure
  sub.hosp.df[,5] == as.numeric(sub.hosp.df[,5]) # Pneumonia  
  "
  
  
  ## Grab only rows with our state value  
  sub.hosp.df <- sub.hosp.df[sub.hosp.df$State==state & sub.hosp.df[outcome] != 'Not Available', ]
  
  " Multiline Comment 
  # Convert mortality data rate to numeric
  sub.hosp.df[,3] == as.numeric(sub.hosp.df[,3]) # Heart Attack
  sub.hosp.df[,4] == as.numeric(sub.hosp.df[,4]) # Heart Failure
  sub.hosp.df[,5] == as.numeric(sub.hosp.df[,5]) # Pneumonia
  "
  
  if(num == "best") { num = 1 }  
  if(num == "worst") { num = nrow(sub.hosp.df) }
 
  if(is.numeric(x=num)) {
    # print(num)
    if(num<1 || num > nrow(sub.hosp.df)) {
      return(NA)
    }
  } else  stop('invalid num')
  
  
  
  #Ranking outcome values 
  # order(as.numeric(sub.hosp.df[,outcome]), sub.hosp.df[,'Hosp_Name']) 
  # - This will give an index of where the value is present starting from lowest 
  # - If there is a tie it will order by Hospital Name
  
  #dataset order by outcome
  #you want all rows ordered in the above order & colums
  #s1<- sub.hosp.df[c(order(as.numeric(sub.hosp.df[,outcome]), sub.hosp.df[,'Hosp_Name'])),]  
  #s2<- s1['Hosp_Name', outcome]
  
  sub.hosp.df <- sub.hosp.df[c(order(as.numeric(sub.hosp.df[,outcome]), sub.hosp.df[,'Hosp_Name'])),]
  
  print(sub.hosp.df$Hosp_Name[num])
  
}
