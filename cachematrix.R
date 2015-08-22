## Functions for caching the inverse of a matrix

## Create object for storing the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      x.inv <- NULL
      set <- function(y) {
            x <<- y
            x.inv <<- NULL
      }
      get <- function() x
      setinv <- function(mean) x.inv <<- solve(x)
      getinv <- function() x.inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Returns inverse of a matrix by the input of the special object

cacheSolve <- function(x, ...) {
      x.inv <- x$getinv()
      if(!is.null(x.inv)) {
            message("getting cached data")
            return(x.inv)
      }
      data <- x$get()
      x.inv <- solve(data, ...)
      x$setinv(x.inv)
      x.inv
}
