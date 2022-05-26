
setwd("c:/Users/noeve/Programing_assignement3")

library(tidyverse)

#outcome <- read_csv("outcome-of-care-measures.csv",
                    #col_types = "ccccccccccdcccccdcccccd") ##This must be improved

#data <- outcome[, c(2, 7, 11, 17, 23)]
#data <- tibble::rowid_to_column(data, "index")
#colnames(data) <- c("num", "Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
######

######
#Outcome <- c("heart attack", "heart failure", "pneumonia")
#State <- unique(data$State)
#Num <- c(1:4706)

#######
#data$Rank <-rank(data[, 4], na.last = TRUE, ties.method = "first")
#Num <- c(1:4706)
my_num <- function(num){
    outcome <- read_csv("outcome-of-care-measures.csv",
                      col_types = "ccccccccccdcccccdcccccd")
    data <- outcome[, c(2, 7, 11, 17, 23)]
    data <- tibble::rowid_to_column(data, "index")
    colnames(data) <- c("N", "Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
    data$Rank <-rank(data[, 4], na.last = TRUE, ties.method = "first")
    Num <- c(1:4706)
    if (num != "best" & num != "worst" & num > 4706) {
        print("NA") 
        stop("number too large, in not in the rank")
    }
    if (num == "best") {
      data <- data[which.min(data$Rank), ]
    }
    if (num == "worst") {
      data <- data[which.max(data$Rank), ]
    }
    if (any(num == Num)) {
      data <- data[which(data$Rank == num), ]
    }
    print(data)
}

my_num("best")
my_num("worst")
my_num(num = 4)
my_num(num = 5000)
