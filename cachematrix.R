## These functions cache a potentially expensive operation of inverting
## a matrix. The 2 work together, makeCacheMatrix and cacheSolve

## makeCacheMatrix takes a matrix as an argument and returns a list
## representing functions to get and set the matrix and to get and set its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve runs the solve operation on a matrix that has been run through makeCacheMatrix
## it returns either the cached solve operation reperesenting the inverse or runs the solve 
## function and saves it in the cache if it doesn't exist in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
