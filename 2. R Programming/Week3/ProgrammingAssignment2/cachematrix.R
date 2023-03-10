## This function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix(, nrow = 1, ncol = 1)) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse ofthe special 
##  "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}