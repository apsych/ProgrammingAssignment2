## cachematrix.R
##
## Create a cache marix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.
##
## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Change the matrix being cached.
##  M <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

## Create a cacheMatrix object for an invertale matrix.

makeCacheMatrix <- function(x = matrix()) {
# initially nothing is cached so set it to NULL
  cachedInverse <- NULL
# store a matrix
  set <- function(y) {
    x <<- y
# since the matrix is assigned a new value, flush the cache
    cachedInverse <<- NULL
  }
# returns the stored matrix
  get <- function() x
# cache the given argument 
  setInverse <- function(inverse) cachedInverse <<- inverse
# get the cached value
  getInverse <- function() cachedInverse
# return a list. Each named element of the list is a function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
# get the cached value
  invFunc <- x$getInverse()
# if a cached value exists return it
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
# else get the matrix, caclulate the inverse and store it in cache
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
# return the inverse
  invFunc
}
