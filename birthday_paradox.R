birthday_paradox <- function() {
    library(dplyr)
    obsdat <- data.frame()
    meanvec <- vector()
    for (i in 1:30) {
        for (x in 1:100){
        y <- sample(1:365, replace = TRUE, i)
        a <- length(unique(y))
        b <- length(y)
            if (a < b) {
                c <- 1
            } else {
                c <- 0
            }
        
        obs <- c(i, c)
        obsdat <- rbind(obsdat, obs)
        colnames(obsdat) <- c("num_birthdays", "duplicate_true")
        by_dup <- group_by(obsdat, num_birthdays)
        output <- summarize(by_dup, mean(duplicate_true))
        }

    }

    output
}
