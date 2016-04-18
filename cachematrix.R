## This file contains the functions required as part of assignment #2 in the R
## programming course.

## makeCacheMatrix <- function(x = matrix())
## This function accepts a matrix and caches its inverse so
## that the inverse is only calculated if the matrix changes.

makeCacheMatrix <- function(x = matrix()) {
        
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invm <<- inv
    getinv <- function() invm
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve <- function(x, ...)
## This functions returns the inverse of the matrix created using
## makeCacheMatrix. If a cached value is available, it is returned, else 
## the inverse is computed and saved for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getinv()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data,...)
    x$setinv(invm)
    invm  
}
