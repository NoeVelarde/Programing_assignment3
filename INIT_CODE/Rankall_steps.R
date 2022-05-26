data2 <- data[, c(1:4)]

data3 <- data2[!is.na(data2$`heart attack`), ]

data_ord <- data3[order(data3$State, data3$`heart attack`, data3$Hospital.Name), ]

data_by <- data_ord %>% 
    group_by(data_ord$State) %>% 
    summarise(State = State,
              Hospital = Hospital.Name,
              `heart attack` = `heart attack`,
              Rank = rank(`heart attack`, na.last = NA, ties.method = "first"), .groups = "drop") %>%
    filter(Rank == 1) %>% 
    View


  data_vevc <- data_ord$`heart attack`
  
  vect2 <- data_ord[4]
  
  St_vect <- data_ord$State
  
  st_out_df <- data.frame(St_vect, data_vevc)
  
  df2 <- cbind(vect2, St_vect)
  colnames(df2) <- c("outcome", "State")
  df2$Rank 
  
    
    
    
    
    
