## cachematrix.R
## (c) Lucas J. Castro

## This script contains two functions to perform a inverse matrix calculation
## using cached values to improve the speed of the running task


## This function creates a special matrix that can cache its inverse calculation

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) m <<- solve
       getinverse <- function() m
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This function computes the inverse of the matrix returned from the above function.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       makeCacheMatrix <- function(x, ...) {
              m <- x$getmean()
              if(!is.null(m)) {
                     message("getting cached data")
                     return(m)
              }
              data <- x$get()
              m <- mean(data, ...)
              x$setmean(m)
              m
       }
}
