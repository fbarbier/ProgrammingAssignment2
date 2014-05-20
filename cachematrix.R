
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # inv store cached inverse matrix
  inv <- NULL
  
  # Set the matrix
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  getMatrix <- function() x
  
  # Set the inverse matrix
  setInverseMatrix <- function(solve) inv <<- solve
  
  # Get the inverse matrix
  getInverseMatrix <- function() inv
  
  # Return the matrix 
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
  # Get the inverse of the matrix
  inv <- x$getInverseMatrix()
  
  # Check if the matrix already exist, return it from the cache
  if (!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  
  # If not : get the inverse of the matrix
  data <- x$getMatrix()
  message("Calculate the inverse of the matrix...")
  inv <- solve(data, ...)
  
  # Cache the inverse of the matrix (Set)
  x$setInverseMatrix(inv)
  
  # Return the inverse matrix
  inv
  
}
