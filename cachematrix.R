## makeCacheMatrix - creates a special matrix that is actually a list containing:
##                          set - set the value of the matrix
##                          get - get the value of the matrix
##                          setsolve - set the value of the matrix inverse
##                          getsolve - get the value of the matrix inverse
## cacheSolve - calculates the inverse of the special matrix created with makeCacheMatrix


## makeCacheMatrix - create the special matrix/list
##       (this is really just a copy of the sample makeVector, with vector replaced by matrix 
##          and mean replaced by solve)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve - get the inverse for the special matrix transformed by makeCacheMatrix
##        (this is a copy of the sample cachemean with mean replaced with solve)
cacheSolve <- function(x, ...) {
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
