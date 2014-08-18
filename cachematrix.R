## Put comments here that give an overall description of what your
## functions do

## The following functions cache the inverse of a matrix. 
## This is done because matrix inversion is usually a costly computation and 
## with caching the value of the inverse, repeated calculation can be avoided. 


## This first function creates a matrix
makeCacheMatrix <- function(x = matrix()) {
     ## Setting the value of the matrix
     new <- NULL
     set <- function(y) {
       x <<- y
       new <<- NULL
     }
     ## Getting the value of the matrix
     get <- function() x
     
     ## Setting the inverse of the matrix
     setinverse <- function(solve) new <<- solve
     
     ## Getting the inverse of the matrix
     getinverse <- function() new
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This second function calculates the inverse of the matrix (given that it is invertible).

cacheSolve <- function(x, ...) {
        ## Returning a matrix that is the inverse of 'x'
  
     ## Getting the inverse of the matrix
     new <- x$getinverse()
     
     ## Returning the cached value if inverse previously calculated
     if(!is.null(new)) {
       message("getting cached data")
       return(new)
     }
     
     ## Calculate the inverse if not previously cached
     data <- x$get()
     new <- solve(data, ...)
     x$setinverse(new)
     new
}
