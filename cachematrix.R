
##   These functions can store an inverse of a matrix for future use

# Set MatrixCache with input Matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvrs <- function(invrs) inv <<- invrs
  getinvrs <- function() inv
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}

# Load CacheMaxtrix 
# Calculate the inverse if there is no Cache
cacheSolve<- function(x, ...) {
  inv <- x$getinvrs()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvrs(inv)
  inv
}

