## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## I have devised a pair of functions that are used to create a special object to store a matrix and caches its inverse.
## This function constructs a special matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function below calculates the inverse of the special matrix constructed by makeCacheMatrix.
## If the inverse has already been computed, with the matrix not changed, then it should retireve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)inv
  inv
}
