## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## 

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # creates a special "matrix" object that can cache its inverse 
    #
    # Args:
    #   x: the matrix
    #
    # Returns:
    #   The cacheable matrix.

    i <- NULL   

    set <- function (y) {
        x <<- y
        i <<- NULL   # clean the cache 
    }
    get <- function () x
    setinverse <- function (inverse) i <<- inverse
    getinverse <- function () i

    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    # Computes and cache the inverse of a special "matrix" returned by makeCacheMatrix above.
    # If the inverse has already been calculated (and the matrix has not changed), then the 
    # cachesolve should retrieve the inverse from the cache.
    # Args:
    #   x:   the cacheable matrix wrapper made with makeCacheMatrix
    #   ...: solve additional arguments
    #
    # Returns:
    #   A matrix that is the inverse of 'x'.

    i <- x$getinverse()
    if (!is.null(i)) {
        message('getting cached data')
        return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}




