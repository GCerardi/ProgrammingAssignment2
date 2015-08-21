## Caching the inverse of a matrix rather than computing it many times can be beneficial,
## as computing the inverse of a matrix might turn out to be a computationally heavy task.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  Lst <- list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  assign("set", set, .GlobalEnv)  
  assign("get", get, .GlobalEnv)    
  assign("setinverse", setinverse, .GlobalEnv)   
  assign("getinverse", getinverse, .GlobalEnv)    
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- get()
  inv <- solve(data, ...)
  setinverse(inv)
  inv
}
