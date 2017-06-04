## Two functions that find the inverse of a matrix and cache the value. 
## If nothing changes to the source matrix, only the cached value is returned.
## Otherwise, the inverse is found, cached, and returned.

## The first function creates a special matrix object so its inverse value can be cached

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse
  getinverse <- function()inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks for a cached inverse value and computes the inverse of the special matrix object above if necessary.

cacheSolve <- function(x, ...) {
        inv_x <- x$getinverse()
  if(!is.null(inv_x)){
    message("getting cached data")
    return(inv_x)
  }
  else{
    data <- x$get()
  inv_x <- solve(data, ...)
  x$setinverse(inv_x)
  inv_x
  }
}
