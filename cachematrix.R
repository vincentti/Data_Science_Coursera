## Functions to calculate and cache the Inverse of a matrix.

## The function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function computes the inverse of the special "matrix" object returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}
