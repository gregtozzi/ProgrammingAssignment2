## cachematrix.R - by Greg Tozzi
## A set of functions that cache the results of matrix inversion
## computations.  In doing so, these functions seek to reduce the
## computational cost of repeated inversion calls.
##
## These functions are based heavily on the examples provided by
## R. Peng.

## makeCacheMatrix generates a list of four functions:
##
## $set() - Sets the matrix to be inverted; when called, this function
## sets the variable m to NULL, indicating that the matrix does not
## yet have a cached inverse.
##
## $get() - Returns the stored matrix.
##
## $setinverse() - Stores the inverse of the stored matrix
## as the variable m.
##
## $getinverse() - Returns the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve performs the following tasks:
## 1. Checks if the invese of the matrix under consideration has
## already been cached; returns the cached inverted matrix if this
## is so.
## 2. Computes the matrix inverse if no cached value exists.  Stores
## this inverse using the $setinverse() function contained in 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
