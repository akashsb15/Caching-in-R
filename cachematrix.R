## Since computing the matrix inverse is computationally intensive,
## caching the inverse once it is computed is a good way to avoid 
## repeated computations over time. This is achieved with two 
## functions as defined below. 

## Note - These functions work under the assumption that the matrix
## is invertible

## makeCacheMatrix(x)
## Function that saves a matrix as list of following functions. 
## set() - sets value of the matrix
## get() - returns value of the matrix
## setinverse() - sets inverse of the matrix
## getinverse() - returns invrese of the matrix

## In short, this function returns an object that can be used to 
## set and get, matrix and inverse values

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


## cacheSolve(x, ...)
## Function that returns the inverse of a matrix that is of 
## makeCacheMatrix type. First checks if the inverse returned 
## by getinverse() is null or not. If null, the inverse is 
## computed, else the cached one is used.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  message("Computed now")
  return(inv)
}

