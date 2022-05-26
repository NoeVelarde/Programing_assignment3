setwd("c:/Users/noeve/Programing_assignement3")

library(tidyverse)

outcome <- read_csv("outcome-of-care-measures.csv",
                    col_types = "ccccccccccdcccccdcccccd") 
# #
data <- outcome[, c(2, 7, 11, 17, 23)]
data <- tibble::rowid_to_column(data, "index")
colnames(data) <- c("N", "Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
# ######
# 
# ######
Outcome <- c("heart attack", "heart failure", "pneumonia")
State <- unique(data$State)
Num <- c(1:400)
# #####
rankall <- function(outcome, num) {
    Outcome <- c("heart attack", "heart failure", "pneumonia")
    State <- unique(data$State)
    Num <- c(1:4706)
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
    #data <- data[!is.na(data[, 4]), ]
    data <- data[order(data$State, data[, 4], data$Hospital.Name, na.last = TRUE), ] # ordering the table
    data <- data %>% 
        group_by(State) %>% 
        mutate(Rank = row_number())
        
   if (num != "best" & num != "worst" & num > 4706) {
       print("NA")
       stop("number too large, in not in the rank")
   }
   if (num == "best") {
       data <- data[which(data$Rank == 1), ]
   }
   if (num == "worst") {
       data <- data[which.max(data$Rank), ]
   }
   if (any(num == Num)) {
       data <- data[which(data$Rank == num), ]
   }
   print(data)
}


#####
rankall("heart failure", 4)
rankall("heart failure", 1)
rankall("heart failure", 3)
rankall("heart attack", "worst")
rankall("pneumonia", "best")
View(rankall("heart attack", "best"))
View(rankall("heart failure", "best"))
View(rankall("pneumonia", "best"))
rankall("heart attack", 5000)
#error_message
head(rankall("heart attack", 20), 10)
rankhospital("NY", "hert attack")

rankhospital("CH", "heart failure")
rankhospital("CA", "stress")