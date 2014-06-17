cube <- function(x,n){
            x^3
}

f<-function(x){
            g<-function(y){
                        y+z
            }
            z<-4
            x+g(x)
}
makevector <- function(x = numeric()){ ## define function and argument init x
            
            m <- NULL   ## init m to a null or empty vector
          
            set <- function(y){  ## construct a function 'set' that take arg 'y'
                        x <<- y  ## special operator that can be used to 
                                 ## assign value different from the current env.
            
                        m <<- NULL
            }
            get <- function() x ## construct a function get that gets val of 'x'
            
            setmean <- function(mean) m <<- mean ## construct a function setmean
                                                 ## that takes the mean as argu
                                                 ## and sets it to 'm' in the  
                                                 ## current env. (parent frame)
            
            getmean <- function() m             ## construct a function getmean
                                                ## to get the valus of m
            
            list(set = set, get = get, setmean = setmean, getmean = getmean)
                                                ## returns the a list of four
                                                ## functions.
}

cachemean <- function(x,...) {      ## define a function 'cachemean' that takes 
                                    ## the arguments 'x' and '...' values of 'x'
            
            m <- x$getmean()        ## brings up the getmean() atomic vector 
                                    ## stored as an attribute of 'x'
            
            if(!is.null(m)) {       ## checks for all non empty 'm' OR in other
                                    ## words checks to see if a mean has already 
                                    ## been cached in 'm'
                        
                        message("getting cached data")
                        return(m)                      
            }
            
            data <- x$get()         ## inits 'data' and stores to get the valuse
                                    ## form the vector x.
            
            m <- mean(data,...)     ## calls the mean function and assigns it to 
                                    ## 'm'
            
            x$setmean(m)            ## sets the mean 'm' to the vector x in the 
                                    ## cached 'x'.
            
            m
}