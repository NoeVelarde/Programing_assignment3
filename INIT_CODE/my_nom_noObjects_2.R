data$Rank<-rank(data[, 4], na.last = TRUE, ties.method = "first")

my_num <- function(num){
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
}

my_num("best")
my_num("worst")
my_num(num = 4)
my_num(num = 5000)
