best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    causes <- c("heart attack", "heart failure", "pneumonia")
    data[, 11] <- as.numeric(data[, 11])
    data[, 23] <- as.numeric(data[, 23])
    data[, 17] <- as.numeric(data[, 17])

	## Check that state and outcome are valid
    if(!state %in% data$State){
        stop("invalid state")
    } else if(!outcome %in% causes){
        stop("invalid outcome")
    } else {
        if(outcome == "heart attack"){
            goal <- data[data$State == state, ]
            attack <- goal[, 11]
            min <- min(attack, na.rm = T)
            index <- which(attack == min)
            hosp_name <- goal[, 2][index]
            # data[, 11]
        } else if(outcome == "heart failure"){
            goal <- data[data$State == state, ]
            failure <- goal[, 17]
            min <- min(failure, na.rm = T)
            index <- which(failure == min)
            hosp_name <- goal[, 2][index]
            # data[, 17]   
        } else { # "pneumonia"
            goal <- data[data$State == state, ]
            pneu <- goal[, 23]
            min <- min(pneu, na.rm = T)
            index <- which(pneu == min)
            hosp_name <- goal[, 2][index]
            # data[, 23]
        }
			## Return hospital name in that state with lowest 30-day death
			## rate
        hosp_name 
    }
}