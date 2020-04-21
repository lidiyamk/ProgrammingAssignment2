## The following functions intend to cache 
##the inverse of a marix

## Function that creates a matrix whose inverse is cacheable

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinvr <- function(solve) invr <<- solve
  getinvr <- function() invr
  list(set=set, get=get, 
       setinvr=setinvr,
       getinvr=getinvr)
}


## This matrix inverses the cached matrix created by the previous function 

cacheSolve <- function(x, ...) {
  invr <- x$getinvr()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$setinvr(invr)
  invr
        ## Return a matrix that is the inverse of 'x'
}
