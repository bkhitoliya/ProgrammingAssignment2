## This function calculates the inverse of the matrix. In order to calculate it there are 2 functions:
## makeCacheMatrix and cacheSolve.
## If the input matrix has not changed since the last time the inverse was 
## calculated, then recompution for inverse is not done and instead return the cached
## version.  If the matrix does change, inverse is recomputed.

## makeCacheMatrix function takes input as a matrix if the matrix is changed by set function then the cache will
## get invalidated. If the input value does not change the value will be displayed from the previously stored value of inverse i.e. cache

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_val) inverse <<- inverse_val
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##cacheSolve function calculates the inverse of the matrix. 
## If the inverse has already been calculated and the matrix was not changed
## since the last run, it returns the cached version of inversion.
## If the inverse has not yet been calculated or the underlying matrix 
## changed since the last call, the value is recomputed using the solve function

cacheSolve <- function(x,...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
}
