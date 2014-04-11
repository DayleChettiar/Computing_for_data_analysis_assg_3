helper <- function(data, outcome, num){
    rank <- data[, 2][order(outcome, data[, 2])[num]]
    rank
}

rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    causes <- c("heart attack", "heart failure", "pneumonia")
    goal <- data[data$State == state, ]
    attack <- as.numeric(goal[, 11])
    failure <- as.numeric(goal[, 17])
    pneumonia <- as.numeric(goal[, 23])
    ## Check that state and outcome are valid
    if(!state %in% data$State){
        stop("invalid state")
    } else if(!outcome %in% causes){
        stop("invalid outcome")
    } else {
        if(num == "best"){
            rank <- best(state, outcome)
        } else{ # num != "best"
            if(outcome == "heart attack"){
                len <- dim(goal[!is.na(attack),])[1]
                if(num != "worst" && num > len){
                    rank <- NA
                } else if(num == "worst"){
                    rank <- helper(goal, attack, len)
                } else{
                    rank <- helper(goal, attack, num)
                }
                # data[, 11]
            } else if(outcome == "heart failure"){
                len <- dim(goal[!is.na(failure),])[1]
                if(num != "worst" && num > len){
                    rank <- NA
                } else if(num == "worst"){
                    rank <- helper(goal, failure, len)
                } else{
                    rank <- helper(goal, failure, num)
                }
                # data[, 17]
            }else{ # "pneumonia"
                len <- dim(goal[!is.na(pneumonia),])[1]
                if(num != "worst" && num > len){
                    rank <- NA
                } else if(num == "worst"){
                    rank <- helper(goal, pneumonia, len)
                } else{
                    rank <- helper(goal, pneumonia, num)
                }
                # data[, 23]
            }            
        }
    
	## Return hospital name in that state with the given rank
	## 30-day death rate
    rank
}