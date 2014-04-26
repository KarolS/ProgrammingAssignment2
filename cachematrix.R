## Functions providing wrappers for matrices that cache inverses of the matrices.


## Creates a wrapper for a matrix, which is a list containing following 4 functions:
## get - to get the wrapped matrix
## set - to set the wrapped matrix (clearing the inverse)
## getinverse - to get the cached inverse of the wrapped matrix (or NULL if not yet calculated)
## setinverse - to set the inverse of the wrapped matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates an inverse of given wrapped matrix. 
## If the wrapper contains a cached inverse, it is returned withour recalculating.
## If it doesn't, the calculated inverse is stored in the wrapper.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        ## message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
