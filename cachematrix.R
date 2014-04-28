## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   # initialize matrix holder
   m <- NULL
   
   # setter function - sets value
   set <- function(y) {
    x <<- y
    m <<- NULL
   }
   
   # getter function - returns value
   get <- function() x
   
   # setter function of inverse of matrix
   setinv <- function(inv) m <<- inv
   
   # getter function of inverse of matrix
   getinv <- function() m
   
   #set all functions
   list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
   
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   # First try to read the inverse matrix of 'x'
   m <- x$getinv()
   
   #if inverse matrix is not null -> inverse already has been calculated
   if(!is.null(m)) {
      message("getting cached data")
      # return the cached inverse and end function cacheSolve
      return(m)
   }
   
   # get data from matrix 'x'
   data <- x$get()
   
   #calculate the inverse matrix of 'x'
   m <- solve(data, ...)
   
   #set the value of inverse matrix of 'x' for later
   x$setinv(m)
   
   # return the inverse matrix of 'x'
   m
}
