## Hello. This .R file contains code for two functions that compute and cache
## the inverse of a given invertible matrix. It was based on the code for
## computing and caching the mean of a vector.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function (y) {
                
                x <<- y
                inv <<- NULL
                
        }
        
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}

## This function computes the inverse of the matrix created above or retrieves
## it from the cache if it was already computed.

cacheSolve <- function(x, ...) {

        inv <- x$getInverse()
        
        if (!is.null(inv)) {
                
                message("getting cached data")
                return(inv)
                
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
        
}
