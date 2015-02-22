## When dealing with large matrices, inverting them may be time or resource intensive. To mitigate this, we'll use caching to store the inverted matrix - that way we only have to invert it again when its value changes, rather than running the same operation with the same result multiple times.
## Example:
## m <- matrix(1:4, nrow = 2, ncol = 2)
## cacheMatrix <- makeCacheMatrix(m)
## m.inverse <- cacheSolve(cacheMatrix)
## m.inverse2 <- cacheSolve(cacheMatrix)

## This function takes a matrix and makes a "cacheable" version of it.
## The return value is a list which contains methods for setting the
## matrix, settings the matrix's inverse, and getting the cached matrix
## or its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Create the variable to hold the inverse
    i <- NULL

    ## Set the matrix cache and clear out the inverse cache, because
    ## if the matrix has changed then the cached inverse is no longer correct.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    ## Get the cached value of the matrix
    get <- function() {
        x
    }

    ## Set the inverse variable, this caches its current value
    setinverse <- function(inverse) {
        i <<- inverse
    }

    ## Get the cached value of the inverse variable
    getinverse <- function() {
        i
    }

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function runs solve() on a matrix, returning its inverse.
## It caches the matrix and its inverse via makeCacheMatrix(), so
## it may be used multiple times with no performance penalty.
cacheSolve <- function(x, ...) {

    ## If we already have a cached inverse, grab it and return it
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    ## Cache miss - get the matrix, inverse it, and cache the result
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)

    ## Now we've cached `m` (the inversed matrix), so go ahead and
    ## return it - next time it will use the cache.
    m
}
