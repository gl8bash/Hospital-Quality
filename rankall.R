rankall <- function(outcome, num = "best" ) {
  
  ## Read outcome data
  hosp.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Store names of state and outcome in a array
  st_tbl <- unique(hosp.df$State)
  
  ## Sort by State 
  st_tbl <- st_tbl[order(st_tbl)] 
  
  ## Store valid outcome names
  outcome_name <- c("heart attack", "heart failure" ,  "pneumonia")

  #Create hospital array for each state
  n <- length(st_tbl)
  hospital <- rep("", length(st_tbl))
  
  ## Check invalid outcome
  if(!any (outcome_name == outcome)) 
    stop("Invalid outcome")
  

  for (i in 1:n ) {
    
    # Get relevant variables from the total dataset
    sub.hosp.df <- subset(hosp.df[,c(2,7,11,17,23)])    
    
    ## Rename name of variables 
    names(sub.hosp.df)[1] <- "Hosp_Name"
    names(sub.hosp.df)[2] <- "State"
    names(sub.hosp.df)[3] <- "heart attack"
    names(sub.hosp.df)[4] <- "heart failure"
    names(sub.hosp.df)[5] <- "pneumonia"  

  ## Clean data for a particular 'State' and 'Outcome' to remove 'NA'
  sub.hosp.df <- sub.hosp.df[sub.hosp.df$State==st_tbl[i] & sub.hosp.df[outcome] != 'Not Available', ] 
  
  ## Sort by Outcome followed by hospital name  
  "
   order(var1, var2)
   s1 <- order(as.numeric(sub.hosp.df[,outcome]), sub,hosp.df[,'Hosp_Name'])
   
   get data ordered by rows like above
   s2 <- sub.hosp.df[c(s1),]
  "
  sub.hosp.df <- sub.hosp.df[c(order(as.numeric(sub.hosp.df[,outcome]), sub.hosp.df[,'Hosp_Name'])),]
  
  ## Accomodating 'best' & 'worst' cases 
  if(num == "best") { num_hosp_rank = 1 }  
  else if (num == "worst") { num_hosp_rank = nrow(sub.hosp.df) }
  else num_hosp_rank = num
  
  hospital[i] <- sub.hosp.df$Hosp_Name[num_hosp_rank]    
 num_hosp_rank = num
  }
 # Return data frome
return(data.frame(hospital=hospital, state=st_tbl))
}
