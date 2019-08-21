#Functions for assessing hospital quality

best <- function(state, outcome) {
  ##Read outcome data
  full <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  by_state <- full[full[7] == state,]
  if (nrow(by_state) == 0) {
    stop("invalid state")
  }

  if (outcome == "heart attack") {
    o_col <- 11
  } else if (outcome == "heart failure") {
    o_col <- 17
  } else if (outcome == "pneumonia") {
    o_col <- 23
  } else {
    stop("invalid outcome")
  }
  
  #Pull out relevant columns
  by_condition <- by_state[,c(2,o_col)]
  by_condition[,2] <- as.numeric(by_condition[,2])
  
  #Remove rows with "NA"
  by_condition <- na.omit(by_condition)
  
  #Reset index and column names
  rownames(by_condition) <- NULL
  colnames(by_condition) <- c("Name", "Var")
  
  #Sort by lowest number of deaths and alphabetically as tiebreaker
  by_condition <- by_condition[order(by_condition$Var,by_condition$Name),]
  #print(head(by_condition))
  
  ## Return hospital w/ lowest mortality
  return(by_condition[1,1])
  
}