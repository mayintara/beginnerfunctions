## Functions

adder <- function (x,y) {
  x + y
}

sub <- function (x,y){
  x - y
}

above <- function(x,n = 10){
  use <- x > n
  x[use]
}

colmean <- function(y, removeNA = TRUE){
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[,i], na.rm = removeNA) 
  }
  means
}

 colmedian <- function(z, removeNA = TRUE) {
   nc <- ncol(z)
   medians <- numeric(nc)
   for(i in 1:nc) {
     medians[i] <- median(z[,i], na.rm = removeNA)
   }
   medians
 }

colmax <- function(max, removeNA = TRUE){
  nc <- ncol(max)
  maxs <- numeric(nc)
  for(i in 1:nc){
    maxs[i] <- max(max[,i], na.rm = removeNA)
  }
  maxs
}

make.power <- function(n) {
  pow <- function(x){
    x^n
  }
  pow
}

make.