## The purpose of the makeCacheMatrix function is to
## create a special "matrix" object that will be able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() {
      x
   }
   setInverse <- function(inverse) {
      inv <<- inverse
   }
   getInverse <- function() {
      inv
   }
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function will compute the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
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
