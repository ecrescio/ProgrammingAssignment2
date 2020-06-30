## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special object that stores a matrix 
## and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)    
  
}


## Write a short comment describing this function
##The following function calculates the inverse of the special "matrix" 
##created with the above function. However, it first checks to see if the
##inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
