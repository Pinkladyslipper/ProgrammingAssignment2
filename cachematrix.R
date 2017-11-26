## Put comments here that give an overall description of what your
## functions do
##These functions are intended to optimize compitations of inverse matrix by caching
## Write a short comment describing this function
##  define the following functions: get and set matrix, extract chache matrix, cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                           
  myset <- function(y) {                     
    x <<- y                             
    m <<- NULL                        
  }
  myget <- function() x                     
  mysetinverse <- function(inverse) m <<- inverse  
  mygetinverse <- function() m                   
  list(myset = myset, myget = myget, mysetinverse = mysetinverse, mygetinverse = mygetinverse)  

}


## Write a short comment describing this function
# extractioan of inverse for our matrix from cache if it was precalculated and matrix was not changed
# in case if not, then calcuklate it inside this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$mygetinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$mysetinverse(m)
  m
}
