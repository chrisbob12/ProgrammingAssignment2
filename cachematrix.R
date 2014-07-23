## makeCacheMatrix creates a special function
## to assign the matrix it receives to a different
## environment, and calculates the inverse of the matrix
## to establish this in the different environment
##
## cacheSolve calls a set of functions established in
## makeCacheMatrix to return the value held in the environment
## populated by makeCacheMatrix
## *******************************************************
## makeCacheMatrix receives an invertible matrix
## establishes two 'external' variables with a special
## function, and sets up a set of internal functions
## which are accessed via a list created in the function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve receives the matrix via the external
## variable set up in makeCacheMatrix and uses list
## calls to retrieve the inverse calculated in
## makeCacheMatrix if the calculation has not been done.
## this is set up by the if test.
## If the calculation has been done, it retrieves the
## value(s) from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
