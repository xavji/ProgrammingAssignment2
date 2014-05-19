## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list of functions to get/set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## set the matrix
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setInverse <- function(newInverse) inverse <<- newInverse
  
  ## get the inverse of the matrix
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## Return a matrix that is the inverse of 'x', caching the result inside x
## x must be "matrix" created with makeCacheMatrix otherwise an error occurs
cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse  
}
