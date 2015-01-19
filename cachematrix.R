## This pair of functions implements a "cached" computation 
## of matrix inversion.
## Since matrix inversion is usually a costly computation,
## it's somewhat beneficial to cache the result of that inversion,
## rather than compute it repeatedly.

## This function creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # function to set matrix object
  set <- function(y)  {
      x <<- y
      inv <<- NULL
  }
  
  # function to get matrix object
  get <- function() x
  
  # function to set inverse object
  setinverse <- function(inverse) {
      inv <<- inverse
  }
  
  # function to get inverse object
  getinverse <- function() inv
  
  # returns list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the "matrix" object
## returned by the 'makeCacheMatrix' function.
## If the inverse has already been calculated and cached,
## the function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  # if inverse is already computed,
  # retrieve it from cache
  if (!is.null(inverse)) {
      message("getting cached inverse matrix")
      return(inverse)
  }
  
  # otherwise, compute and cache it before returning
  origmatrix <- x$get()
  inverse <- solve(origmatrix)
  x$setinverse(inverse)
  inverse
}
