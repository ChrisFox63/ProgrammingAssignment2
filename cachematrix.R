## Here is a pair of functions that cache the inverse of an invertible matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## To reset a matrix
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## To get the matrix assigned by the 'x' argument
        getMatrix <- function() x
        
        ## To assign an inverse to the 'inv' variable which also cache it
        setInverse <- function(inverse) inv <<- inverse
        
        ## To get the result of inverse
        getInverse <- function() inv
        
        ## Output a list containing 4 functions which are defined above
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## the function above. If the inverse has already been calculated and the matrix
## has not changed, this function would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        
        ## If the inverse of 'x' is not null, return the inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## When the inverse is null, compute the inverse and set it into cache
        data <- x$getMatrix()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
