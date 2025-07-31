## functions that store matrices' inverse in cache and 
## calculate inverse if not present in cache

## create "matrix" to set vlaue of matrix, get value of matrix,
## set mean, get mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m 
  list(set = set, get = get,
       setinv = setinv, getinv=getinv)
}


## check to see if mean calculated (in cache), and if not
## calculate and store in cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data,...)
  x$setinv(x)
  m
}
