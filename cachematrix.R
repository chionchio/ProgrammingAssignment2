## Functions that cache the inverse of a matrix
## Usage:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## Creates a special "matrix", which is really a list containing a function to
## 
## set the value of the matrix
## get the value of the matrix
## set the value of the inv
## get the value of the inv
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## function to set matrix.
  setMatrix <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  
  ## function to get matrix
  getMatrix <- function() x
  
  ## function to set the inverse
  setinverse <- function(inv) inverse <<- inv
  
  ## function to get the inverse
  getinverse <- function() inverse
  
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Function calculates the inverse of the special "matrix" created with the above function
cacheSolve <- function(x, ...) {
  ## Gets the cache value if exists.
  inverse <- x$getinverse()
  
  ## returns it if not NULL.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Creates a new value and stores in cache.
  matrix <- x$getMatrix()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  
  ## Returns new value.
  return(inverse)
}
