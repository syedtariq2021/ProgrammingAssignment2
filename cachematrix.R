## A pair of functions (1) makeCacheMatrix - to create a special matrix
## which cache its inverse the first time it is called; (2) cacheSolve - which returns 
## the inverted matrix calculated fresh or 
## from cache if it has already been calculated.

## makeCacheMatrix creates the special matrix with 
## pointers for functions in its environment, so cacheSolve 
## can execute them

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(inverted) m <<- inverted
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## cacheSolve - when called the first time
## with an invertible matrix (not checked, if invertible)
## returns the inverse (using the function solve), and
## caches the resulting inverse 
## if called again, checks to see if inverse is available
## and returns if available

cacheSolve <- function(x, ...) {
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
}
