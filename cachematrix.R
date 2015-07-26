## Pair of functions to cache the inverse of a matrix to avoid computing it repeatedly.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated, it will retrieve it from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("using cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}