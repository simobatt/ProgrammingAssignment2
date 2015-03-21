# Matrix inversion is usually a costly computation and so there may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a special "matrix", which is a list containing
# a function to:
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of inverse of the matrix
#   4. get the value of inverse of the matrix

# Usage example:
#
# > source('cachematrix.R')
# > m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
  
  # holds the cached inverse of the matrix or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  cachedInv <- NULL
  
  # set the matrix
  setMatrix <- function(y) {
    x <<- y
    # since a new value is assigned to the matrix, the cache is flushed
    cachedInv <<- NULL
  }
  
  # get the matrix
  getMatrix <- function() x
  
  # cache the given inverse matrix
  setInverse <- function(i) cachedInv <<- i
  
  # get the cached inverse matrix
  getInverse <- function() cachedInv
  
  # return a list where each element is a function
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}



# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# getInverse function.

# This function assumes that the matrix is always squared and invertible.

cacheSolve <- function(x, ...) {
  
  # get the cached matrix
  inv <- x$getInverse()
  
  # if a cached value exists return it
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # otherwise get the matrix and calculate the inverse
  m <- x$getMatrix()
  inv <- solve(m, ...)
  
  # store the calculated inverse matrix in the cache
  x$setInverse(inv)
  
  # return the calculated inverse matrix
  inv
  
}