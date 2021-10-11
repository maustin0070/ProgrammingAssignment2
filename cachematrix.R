## Functions to inverse matrix, cache result, and return result from cache

## Cache function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function to return a matrix that is the inverse of 'x'

cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("gettin' that cached data, yo!")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat, ...)
  x$setinverse(inv)
  inv
}