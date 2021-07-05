## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
      x <<- y
      xinv <<- NULL
  }
  get <- function() x
  setinverse <- function(x_inverse) xinv <<- x_inverse
  getinverse <- function() xinv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      xinv <- x$getinverse()
      if(!is.null(xinv)) {
          message("getting cached data")
          return(xinv)
        
      }
      matrix_x <- x$get()
      xinv <- solve(matrix_x, ...)
      x$setinverse(xinv)
      xinv
}
