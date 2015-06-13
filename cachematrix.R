## Put comments here that give an overall description of what your
## functions do

##   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##  Changing environments for scoping
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  ##  get when object already exists
  get <- function() x
  ##  set ehen object must be computed by function
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
##    has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible 
##     matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
## Test if cached data exists, if so get
  if(!is.null(m)) {
      message("getting cached data")
##  exit function returning cached data
      return(m)
  }
##  cached object not found, compute
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
