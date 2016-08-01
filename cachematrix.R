## Performance-optimized, cache-able library for computing inverse of matrices
## For running accompanying tests, please refer to README.md

## A special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv      <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() {
    inv
  }
  
  list(set        = set, 
       get        = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Caching matrix inversion function
cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'

  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}