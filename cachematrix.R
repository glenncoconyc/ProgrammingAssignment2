## Put comments here that give an overall description of what your
## functions do

## This function is used to get, set a matrix and its inverse from the cache. It uses the << operator to assign variables that are not defined in its environment

makeCacheMatrix <- function(x = matrix()) {
  
  InverseMat <- NULL
  set <- function(y) {
    x <<- y
    InverseMat <<- NULL
  }
  
  #returns the matrix
  get <- function() x
  setInverse <- function(Inv) InverseMat <<- Inv
  
  #return the inverse of the matrix from the cache (if inverse doesn't exist, return null)
  getInverse <- function() InverseMat
  
  #return a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## The function returns the inverse of a matrix. It first tries to retrieve the inverse from the cache. If the inverse is not found in the cache, it gets the inverse using the solve function and sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  InverseMat <- x$getInverse()
  
  #Checks if the inverse already exists in the cache
  if(!is.null(InverseMat)) {
    message("getting cached data")
    return(InverseMat)
  }
  data <- x$get()
  
  
  #Calculates the inverse of the matrix using the solve function
  InverseMat <- solve(data, ...)
  
  #sets the inverse in the cache
  x$setInverse(InverseMat)
  InverseMat
}
