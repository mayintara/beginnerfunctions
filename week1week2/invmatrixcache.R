## Assignment 2 Lexical Scoping & Matrix Inversion.
## Rahul Garkhail a.k.a mayintara email: garkhail.rahul@gmail.com


## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {     ## defimes a function 'makeCache
                                                ## Matrix' 
            
            Inv <- NULL                         ## Initiates Inv to an empty
                                                ## set.
            
            set <- function(z) {                ## constructs a function 'set'to
                                                ## set the values of the x and 
                                                ## Inv into the cache. 
                        x <<- z
                        Inv <<- NULL
            }
            
            get <- function() x                 ## constructs a function that 
                                                ## gets the value of 'x'.
            
            setInv <- function(solve) Inv <<- solve         ## constructs a 
                                                            ## setInv funcion
                                                            ## to set inv
            
            getInv <- function() Inv            ## gets the Inv matrix
            
            list(set = set, get = get, setInv = setInv, getInv = getInv)
                                                ## returns a list of 4 functions            
}


## 2. cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.


cacheSolve <- function (x, ...){
            
            Inv <- x$getInv()                   ## gets the cached Inv
            
            if(!is.null(Inv)){                  ## check if cacheInv is not null
                        
                        message("getting cached data")
                        
                        return(Inv)             ## returns Inv fro the cache
            }
            
            data <- x$get()                     ## if the 'if(!is.null(Inv)) is
                                                ## not true in other words 
                                                ## Inv = NULL - then 'data' gets
                                                ## matrix x
            
            Inv <- solve(data, ...)             ## 'solve' matrix x and since 
                                                ## there are no 'b' matrix or 
                                                ## Right Hand Values the 'b' 
                                                ## matrix is considered as an 
                                                ## identity matrix and therefore 
                                                ## the solution of 'a*b' matrix 
                                                ## multiplication using 'solve'
                                                ## function results in an INV.
                                                ## (look at ?Solve)
                                                ## the same can be used in 
                                                ## Linear OPtimization also.
            
            x$setInv(Inv)                       ## Sets the Inv to Cache
            
            Inv
}