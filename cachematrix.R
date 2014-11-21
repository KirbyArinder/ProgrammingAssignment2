## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  calcinverse <- NULL
  set <- function(passmatrix) {
    mymatrix <<- passmatrix
    calcinverse <<- NULL
  }
  get <- function() mymatrix
  setinverse <- function(passinverse) {
    calcinverse <<- passinverse
  }
  getinverse <- function() calcinverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  calcinverse <- x$getinverse()
  if(!is.null(calcinverse)){
    message("getting cached data")
    return(calcinverse)
  }
  mymatrix <- x$get()
  calcinverse <- solve(mymatrix, ...)
  x$setinverse(calcinverse)
  calcinverse
        ## Return a matrix that is the inverse of 'x'
}
