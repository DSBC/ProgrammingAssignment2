## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix - x
# set the inverse of the matrix
# get the inverse of the matrix - inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function. This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("cache hit")
    return(inv)
  }
  m <- x$get()
  message("cache miss")
  inv <- solve(m)
  x$setinverse(inv)
  inv
}