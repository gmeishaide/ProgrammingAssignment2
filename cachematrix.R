## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create cache inverse matrix by initializing the vector plots for variables c, d, and i by
## setting up the inverse matrix for cache using the setinverse function and then creating
## the function to pull vector data list  from the cache is called getinverse.

makeCacheMatrix <- function(c = matrix()) {
  i <- NULL
  set <- function(d) {
    c <<- d
    i <<- NULL
  }
  get <- function() c
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function
## The cacheSolve function reads the data from the
## getinverse function once the data has been retrieved
## from cache matrix

cacheSolve <- function(c, ...) {
  ## Return a matrix that is the inverse of 'c'
  i <- c$getinverse()
  if (!is.null(i)) {
    message("retreiving cached data")
    return(i)
  }
  data <- c$get()
  i <- solve(data, ...)
  c$setinverse(i)
  i
}
