## Assignment Module 2 Week 3 by Rene de Jong
## Assignment: write a pair of functions that cache the inverse of a matrix.
## Assumption: the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL

  fsetmat <- function(newmat) {
    x <<- newmat
    minv <<- NULL
  }
  fgetmat <- function() x
  fsetinverse <- function(im) minv <<- im
  fgetinverse <- function() minv

  list(set = fsetmat, 
       get = fgetmat,
       setinverse = fsetinverse,
       getinverse = fgetinverse
      ) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inversematrix <- x$getinverse()
  
  if(!is.null(inversematrix)) {
    message("get cached inverse")
    return(inversematrix)
  }
  
  data <- x$get()
  inversematrix <- solve(data,...)
  x$setinverse(inversematrix)
  inversematrix
}
