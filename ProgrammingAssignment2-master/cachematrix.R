## Caching the Inverse of a Matrix:
## Matrix Inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than cmpute
## it repeatedly.
## Here are two functions that are used to create a special object
## that stores a matrix and cache its inverse.

makeMatrix <- function(x = matrix()) {
  cached <- NULL
  set <- function(y) {
    x <<- y
    cached <<- NULL
  }
  get <- function() x
  ## This method determine whether inverse already been calculated
  ## If computed, return it, else caculate it and save it in cached.
  getInverse <- function() {
    if (!is.null(cached)) {
      message("using cached data")
      return(cached)
    }
    cached <<- solve(x)
    return(cached)
  }
  list(set = set,
       get = get,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


