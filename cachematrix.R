## Matrix inversion is usually a costly computation.  This pair of functions cache the inverse of a matrix
## to eliminate the unnecessary repetition of time-consuming computations when a matrix has not changed.


makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse,
    ## which is really a list containing a function to
    ##      set the value of the matrix
    ##      get the value of the matrix
    ##      set the value of the matrix inverse
    ##      get the value of the matrix inverse
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
    ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
    ## If the inverse has already been calculated (and the matrix has not changed), 
    ## then the cacheSolve retrieve the inverse from the cache.
    ## Otherwise, it calculates the inverse using solve() and cache the result.
    ## It return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
