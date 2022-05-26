pn <- data[, -c(4,5)]
browseURL("https://cran.r-project.org/web/packages/dplyr/vignettes/base.html")

####
mtcars_by <- by(mtcars, mtcars$cyl, function(df) {
    data.frame(cyl = df$cyl[[1]], mean = mean(df$disp), n = nrow(df))
})
do.call(rbind, mtcars_by)

#####
pn_data <- by(pn, pn$State, function(pn) {
    data.frame(State = pn$State[[1]], Rank = rank(pn$pneumonia), n = nrow(pn))
})
do.call(rbind, pn_data)
pn <- pn[order(pn$State, pn$pneumonia, pn$Hospital.Name, na.last = TRUE), ] # ordering the table
pn2 <- as.data.frame(do.call(rbind, pn_data)) # to convert the list to data frame
pn2 <- tibble::as.tibble(pn2) # to tibble object
pn_all <- cbind(pn, pn2) # one sole data.frame
pn_all <- pn_all[, -5] # taking out the duplicate column "State"
pn_all <- tibble::as.tibble(pn_all) # to tibble object
best_all <- pn_all[which(pn_all$Rank == 1), ] # the best rank along all States

