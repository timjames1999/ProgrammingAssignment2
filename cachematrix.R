## The below are a pair of functions that cache the inverse of a matrix.

## This function creates a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}



## The second function calculates the inverse of the matrix created 
## with the first function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the mean from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
