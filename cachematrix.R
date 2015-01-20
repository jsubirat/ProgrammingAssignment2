# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# To use it, simply create a normal matrix and convert it into this special object by using the
# makeCacheMatrix method below. Example:
#
# m <- matrix(1:4, 2, 2)
# cache_m <- makeCacheMatrix(m)   # cache_m is now an object containing a matrix with inverse caching capabilities
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Sets the value of the matrix (used to modify its value)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Gets the matrix
  get <- function() x
  # Sets the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  # Gets the value of the inverse of the matrix (if available, otherwise returns NULL)
  getinverse <- function() inv
  # This list is returned when makeCacheMatrix is called. From then, the methods above can be accessed as cache_m$method
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves
# the inverse from the cache
cacheSolve <- function(x, ...) {
  # Get the cached value of the inverse of the matrix (if not cached, it returns NULL)
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # If it is not NULL, it means that it was calculated before. Return, then, the cached inverse value.
    message("getting cached data")
    return(inv)
  }
  # If it is NULL, the original data is retrieved from the cached matrix object,
  data <- x$get()
  # the inverse is calculated
  inv <- solve(data, ...)
  # cached into the cached matrix object
  x$setinverse(inv)
  # and finally returned to the user.
  inv
}