## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse as NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y  # Update the matrix with the new value
    inv <<- NULL  # Reset the cached inverse
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the cached inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to manipulate the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Finished

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")  # Message indicating that the inverse is cached
    return(inv)  # Return the cached inverse
  }
  
  # If the inverse is not cached, calculate it
  data <- x$get()  # Retrieve the matrix
  inv <- solve(data, ...)  # Compute the inverse using the solve function
  x$setInverse(inv)  # Cache the computed inverse
  
  inv  # Return the computed inverse
}


## Example

# Create a special matrix object using makeCacheMatrix
a <- makeCacheMatrix(matrix(c(4, 7, 2, 6), 2, 2))  # Example 2x2 matrix
a$get()
# Compute and cache the inverse
cacheSolve(a)  # This will compute the inverse

# Retrieve the cached inverse (no computation this time)
cacheSolve(a)  # This will retrieve the cached inverse and print "getting cached data"
