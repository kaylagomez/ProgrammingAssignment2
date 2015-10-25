## The makeCacheMatrix and cacheSolve together cache a square matrix and its inverse 
## to be called later instead of recalculated. The parameter 'x' is a square matrix 
## which has an inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## This function takes in a parameter that is a square matrix and caches the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<-solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  ## This function returns the inverse of a square matrix or caches the 
  ## solution if the inverse has already been computed before
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
