library(tidyverse)

outcome <- read_csv("outcome-of-care-measures.csv",
                    col_types = "ccccccccccdcccccdcccccd") ##This must be improved

data <- outcome[, c(2, 7, 11, 17, 23)]

colnames(data) <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
######

######
Outcome <- c("heart attack", "heart failure", "pneumonia")
State <- unique(data$State)

#####3
best <- function(state, outcome) {
        if (!any(state == State)) stop("invalid state")
        if (!any(outcome == Outcome)) stop("invalid outcome")
        if (outcome == "heart attack"){
          data <- data[, c("Hospital.Name", "State", "heart attack")]
        }
        if (outcome == "heart failure"){
          data <- data[, c("Hospital.Name", "State", "heart failure")]
        }
        if (outcome == "pneumonia"){
          data <- data[, c("Hospital.Name", "State", "pneumonia")]
  }  
        data <- data[data$State == c(state), ]
        data <- data[order(data[, 3], data$Hospital.Name, na.last = TRUE), ]
        data <- data[1, 1]
        print(data)
}
###
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")

#error_message
best("BB", "heart attack")
best("NY", "hert attack")

best("CH", "heart failure")
best("CA", "stres")
