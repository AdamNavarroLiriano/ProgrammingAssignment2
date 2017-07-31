## These functions calculate the inverse of a invertible matrix and cache the
## resulting matrix, so that it can be accessed later

## creates a special object that can cache the inverse of the matrix

makeCacheMatrix <- function(aMatrix = matrix()) {
  inv <- NULL
  set <- function(Matrix){
    aMatrix <<- Matrix
    inv <<- NULL
  }
  getMatrix <- function() aMatrix
  setinverse <- function(inverse){
    inv <<- inverse
    }
  getinverse <- function() inv
  list(set = set, getMatrix = getMatrix, setinverse = setinverse, getinverse = getinverse)
  
}


## Computes the inverse, but first checks to see if it's been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(!is.null(x$getinverse())){
    message("Retrieving inverse ")
    x$getinverse()
  }
  Matrix <- x$getMatrix()
  inverse <- solve(Matrix, ...)
  x$setinverse(inverse)
  inverse
}
