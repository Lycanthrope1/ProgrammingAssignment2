##  These functions store the inverse of a matrix, so that it needn't be recalculated 
##  every time it is used.

##  This function provides a list of functions that may be operated by cacheSolve to 
##  cache the inverse of the matrix, x, input to the function.  By default, it defines
##  x to be the empty matrix.

makeCacheMatrix <- function(x = matrix()) {
## This initializes the inverse as NULL in order to wipe out a previously cached inverse if there is one. 
  inv <- NULL 
## This function, set, changes x, so that in the context of cacheSolve, it can be changed by the input. 
  set <- function(Y) {
    x <<- Y 
    inv <<-NULL
  }
## The function get returns the value of the matrix for which the inverse is to be determined and cached.
  get <- function() x
## This chaches the value of inverse as inv within the parent environment, i.e. within cacheSolve, when 
## cacheSolve determines it.
  setinverse <- function(inverse_matrix) inv <<- inverse_matrix 
## This retrieves the cached inverse.
  getinverse <- function() inv
## This stores each of the above functions within a list, corresponding to a variable assigned the 
## makeCacheMatrix function. 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the function input to makeCacheMatrix.  If the inverse hasn't
## already been cached, it calculates and caches it.  If it has been cached, it simply returns the 
## cached inverse.  Additionally, if a second argument, M, is provided which is an invertible matrix,
## it will redefine the "special" matrix from makeCacheMatrix as that M.  

## Note that x must be a makeCacheMatrix stored in a variable, i.e. x must be initialized before running 
## the function.
cacheSolve <- function(x , M = FALSE, ...) {
## Checks for a second argument.  Suppresses warnings associated with the fact that it only checks the 
## first element. 
  suppressWarnings(if (M != FALSE) {
    x$set(M)
  })
##  Retrives the inverse from makeCacheMatrix.  Will be null if this function hasn't already run for this
## matrix.
  inv <- x$getinverse()
## If it is not null, call's up the cached version.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## If it is null, gets the matrix, calculates the inverse, and caches it. 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  ## Returns a matrix that is the inverse of 'x'
  inv       
}
