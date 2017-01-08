## The aim of our two functions is to create a matrix
## and take the inverse of said matrix

## makeCacheMatrix sets a matrix for the value x
## to be referenced in the cacheSolve function

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
       setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of a matrix supplied above
## The function will retrieve the inverse from
## the cache if the inverse has already been calculated

cacheSolve <- function(x, ...) {
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
