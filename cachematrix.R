## Function 'makeCacheMatrix' creates a special matrix object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Default value of the inverse before being calculated
  i <- NULL
  
  ## Function 'set' for setting the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Function 'get' for getting the value of the matrix
  get <- function() x
  
  ## Function 'setinverse' for setting the value of the inverse
  setinverse <- function(solve) i <<- solve   
  
  ## Function 'getinverse' for getting the value of the inverse
  getinverse <- function() i
  
  ## Create a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function 'cacheSolve' calculates the inverse of the special
## matrix returned by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  ## Get the inverse of the matrix from the cache
  i <- x$getinverse()
  
  ## Check if the inverse has already been calculated
  if(!is.null(i)) {
    
  ## Return the cached inverse and skip the computation
    return(i)
  }

  ## Otherwise get the matrix
  m <- x$get()
  
  ## Then calculate the inverse of the matrix
  i <- solve(m, ...)
  
  ## Save the calculation in the cache
  x$setinverse(i)
  
  ## Finaly return the newly calculated inverse
  i
}
