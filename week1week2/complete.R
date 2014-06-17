## Assignment 1 - Part 2 'Complete'
## Rahul Garkhail a.k.a mayintara <garkhail.rahul@gmail.com>

complete <- function(directory, id = 1:332) {
  files <- dir(directory, full.names = TRUE)
  data <- data.frame()
  nobs <- NULL
  for (i in id){
       data <- read.csv(files[i])
       sumocc <- sum(complete.cases(data))
       nobs <- c(nobs, sumocc)
  }
  dcomp <- data.frame(id,nobs)
  dcomp
}