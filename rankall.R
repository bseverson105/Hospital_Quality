##Function to get hospital ranking in certain outcomes for all states

rankall <- function(outcome, num = "best") {
  ##Read outcome data
  full <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ##Get all the unique states
  states <- as.data.frame(unique(full[,7]))
  print(dim(states))
  output <- setNames(data.frame(matrix(ncol = 2, nrow = nrow(states))),c("hospital","state"))
  print(output)
  for (state in states) {
    print(state)
  }
  
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
  
  ## Handle 'best' and 'worst' inputs
  if (num == "best") {
    num <- 1
  } else if (num == "worst"){
    num <- nrow(by_condition)
  }
  
  ## Return hospital w/ appropriate rank
  return(by_condition[num,1])
}