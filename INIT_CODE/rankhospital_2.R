setwd("c:/Users/noeve/Programing_assignement3")

library(tidyverse)
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


data <- outcome[, c(2, 7, 11, 17, 23)]
data <- tibble::rowid_to_column(data, "index")


colnames(data) <- c("N", "Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
####
data <- as_tibble(data)

######
data$`heart attack` <- as.numeric(data$`heart attack`)
data$`heart failure` <- as.numeric(data$`heart failure`)
data$pneumonia <- as.numeric(data$pneumonia)
######
Outcome <- c("heart attack", "heart failure", "pneumonia")
State <- unique(data$State)
Num <- c(1:4706)
#####3
rankhospital <- function(state, outcome, num) {
    if (!any(state == State)) stop("invalid state")
    if (!any(outcome == Outcome)) stop("invalid outcome")
    if (outcome == "heart attack"){
        data <- data[, c("N", "Hospital.Name", "State", "heart attack")]
    }
    if (outcome == "heart failure"){
        data <- data[, c("N", "Hospital.Name", "State", "heart failure")]
    }
    if (outcome == "pneumonia"){
        data <- data[, c("N", "Hospital.Name", "State", "pneumonia")]
    }  
    data <- data[data$State == c(state), ]
    data <- data[order(data[, 4], data$Hospital.Name, na.last = TRUE), ]
    data$Rank<-rank(data[, 4], na.last = "keep", ties.method = "first")
    out_data <- data
    if (num != "best" & num != "worst" & num > 4706) {
        print("NA") 
        stop("number too large, in not in the rank")
    }
    if (num == "best") {
        out_data <- out_data[which.min(out_data$Rank), ]
    }
    if (num == "worst") {
        out_data <- out_data[which.max(out_data$Rank), ]
    }
    if (any(num == Num)) {
        out_data <- out_data[which(out_data$Rank == num), ]
    }
    print(out_data)
}

###
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("TX", "heart failure")
rankhospital("MD", "heart attack")
rankhospital("MN", "heart attack")

#error_message
rankhospital("BB", "heart attack")
rankhospital("NY", "hert attack")

rankhospital("CH", "heart failure")
rankhospital("CA", "stres")