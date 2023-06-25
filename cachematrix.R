


## This function creates a matrix object that caches its inverse.
## "x" is the matrix object submitted to the console.

makeCacheMatrix <- function(x = matrix()) {
h <- NULL
        set <- function(y) {
        x <<- y
        h <<- NULL
  }
get <- function() x
setsolve <- function(solve) h <<- solve
getsolve <- function() h
list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

## This function computes the inverse of the matrix object created by 
## makeCacheMatrix. If the inverse is already calculated (and the 
## matrix has not been invalidated), then it should retrieve the inverse
## from the cache.


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of "x"
h <- x$getsolve()
        if(!is.null(h)) {
        message("getting inversed matrix")
        return(h)
  }
        data <- x$get()
        h <- solve(data, ...)
        x$setsolve(h)
        h
}
