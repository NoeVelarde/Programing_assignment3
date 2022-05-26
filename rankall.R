setwd("c:/Users/noeve/Programing_assignement3")

library(tidyverse)

outcome <- read_csv("outcome-of-care-measures.csv",
                    col_types = "ccccccccccdcccccdcccccd") 

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
    data <- data[!is.na(data[, 4]), ]
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
    Num_t <- c(1:54)
    df_st <- data.frame(Num_t, State)
    df_st <- df_st[order(df_st$State, na.last = TRUE), ]
    data2 <- tibble::as_tibble(df_st)
    merged_df <- merge(data, data2, all.y = TRUE)
    spected_df <- merged_df[, c(1,3,4,1)]
    print(spected_df)
}


#####
rankall("heart failure", 4)
rankall("heart failure", 1)
rankall("heart failure", 3)
rankall("heart attack", "worst")
head(rankall("heart attack", 20), 10)
rankall("pneumonia", "best")
tail(rankall("pneumonia", "worst"), 3)
rankall("heart attack", 5000)
#error_message
rankhospital("BB", "heart attack")
rankhospital("NY", "hert attack")

rankhospital("CH", "heart failure")
rankhospital("CA", "stress")