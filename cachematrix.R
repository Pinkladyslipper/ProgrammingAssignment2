# Code for inverse of a matrix is below.
# It replicates the example "Caching the Mean of a Vector"
# Substituting for makeVector with makeCacheMatrix, 
# numeric with matrix, 
# m with inv, mean with inverse  
# cachemean with cacheSolver, which obligates 
# Subsituting the subsequent mean with Solve, which is in the instructions forsquare invertible matrix

# THe function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                    
  set <- function(y) {              
    x <<- y                           
    inv <<- NULL                      
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)                                                                           
}

# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) # instruction is to use "solve" function that can be used when x is an square inverible matix, which is to be assumed in this assignment
  x$setinverse(inv)
  inv
}

# Test the function with the two code lines below:
a <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(a)

