## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation so if we make it into cache memory so it will be less time consuming

## This function helps us to create a"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                            inv <- NULL
                            set <- function(y) {
                                      x <<- y
                                      inv <<- NULL
                                    }
                            get <- function() x
                            setInverse <- function(inverse) inv <<- inverse
                            getInverse <- function() inv
                            list(set = set,
                                 get = get,
                                 setInverse = setInverse,
                                 getInverse = getInverse)
                          
                          }


## If the inverse of the matrix has already been calculated (and the 
## matrix has not changed), then this function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
                      ## Return a matrix that is the inverse of 'x'
                      inv <- x$getInverse()
                      if (!is.null(inv)) {
                          message("getting cached data")
                          return(inv)
                          }
                      mat <- x$get()
                      inv <- solve(mat, ...)
                      x$setInverse(inv)
                      inv
                    }