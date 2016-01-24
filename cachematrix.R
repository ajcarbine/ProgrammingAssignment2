## makeCacheMatrix function can be used to set the inverse of a given matrix, or
##     get it if the calculation has already occurred, and it has been cached
## cacheSolve finds the inverse of a matrix, if the inverse does not exist, using
##     methods in makeCacheMatrix to determine if the inverse is cached

## Creating a matrix which can cache its own inverse; delivers a list of methods

makeCacheMatrix <- function(x = matrix()) {
    invrc <- NULL
    set <- function(y) {
        x <<- y
        invrc <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invrc <<- inverse
    getInverse <- function() invrc
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Solves for the inverse of a matrix, or uses the inverse from a cache when available

cacheSolve <- function(x, ...) {

    ## See if inverse is already available in cache, return/end function if so
    
    invrc <- x$getInverse()
    if(!is.null(invrc)) {
        message("getting cached data")
        return(invrc)
    }
    
    ## Otherwise get data and solve for inverse, return "invrc"
    data <- x$get()
    invrc <- solve(data, ...)
    x$setInverse(invrc)
    invrc
}