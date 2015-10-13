## Assignment 2 for coursera course. 
## Functions for creating cached inverse of a matrix

## This functions creates a 'special' matrix which inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
    invcach <- NULL
    set <- function(y) {
        x <<- y
        invcach <<- NULL
     }
    get <- function() x
    setinv <- function(inv) invcach <<- inv
    getinv <- function() invcach
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}



## This functions retrieves the cached inverse or calculate the inverse if non cached value exist
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    invcach <- x$getinv()
    if(!is.null(invcach)) {
        message("getting cached data")
        return(invcach)
    }
    data <- x$get()
    inv <- solve(data, ...)
     x$setinv(inv)
    inv
}
