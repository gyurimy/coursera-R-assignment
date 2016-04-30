## makeCacheMatrix : 
## This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invofm <- NULL
  set <-function(y) {
    x <<- y
    invofm <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invofm <<- inverse
  getinverse <- function() invofm
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve : 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R.
## Assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invofm <- x$getinverse()
  if(!is.null(invofm)) {
    message("getting cached data")
    return(invofm)
  }
  data <- x$get()
  invofm <- solve(data, ...)
  x$setinverse(invofm)
  invofm
}

