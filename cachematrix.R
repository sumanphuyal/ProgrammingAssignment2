## Programming Assignment 2
## 2014-08-24

## There are two functions in this file - makeCacheMatrix and cacheSolve. 
## These function are responsible for calculating 
## inverse of invertible matrix. Solve() funtionc in R is used to calculate
## inverse. While  calculating inverse of a matrix it will first check
## if the inverse is already available in cahce. If it is not available in cache, inverse will
## be calculated but if inverse is already availabe in cache, it will just be taken from there
## instead of calculating again. Lexical scoping is used along with <<- operator to check
## whether inverse already exist in cache. 

##############################################################################
##                     makeCacheMatrix                                    ####
##############################################################################

## This function creates a special "matrix" object that
## can cache its inverse.This function will create a list 
## containg four different function namely set, get, setInverse, getInverse. set()
## will set matrix, get() will get matrix, setInverse() will set inverse of matrix 
## and getInverse() get inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) 
    {
      x <<- y
      s <<- NULL
      
    }
    get <- function() x
    setInverse <- function(solve) s <<- solve
    getInverse <- function() s
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
##############################################################################
##                    cacheSolve function                                 ####
##############################################################################

## This function computes the inverse of the special :matrix" 
## returned by makeCacheMatrix above. If the inverse
## has already been calculated ( and the matrix has not changed),
## then the cachesolve will return the inverse from the cache.
## When this function is called if it displays message "Getting cached data"
## that means inverse was already calculated and it is just getting values from
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <-x$getInverse();
    if(!is.null(s))
    {
      message("Getting cached data")
      return(s)      
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setInverse(s)
    s
}


## These calls can be uncommented to test above code. 
## mat = matrix( c(2,4,3,1), nrow=2, ncol=2)

##solve(mat)
## matrixObject <- makeCacheMatrix(mat)
##matrixObject$get()
##matrixObject$getInverse()
##cacheSolve(matrixObject)
##cacheSolve(matrixObject)

