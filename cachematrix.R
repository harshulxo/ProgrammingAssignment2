## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        nv <- NULL  # Initialize the inverse as NULL
  
  # Set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when the matrix is set
  }
  
  # Get the matrix value
  get <- function() x
  
  # Set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to interact with the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)  # Return cached inverse
  }
  
  # Compute the inverse if not cached
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse using the solve function
  
  x$setInverse(inv)  # Cache the computed inverse
  inv  # Return the inversecd
        ## Return a matrix that is the inverse of 'x'
}
