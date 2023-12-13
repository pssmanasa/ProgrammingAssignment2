## The two functions (makeCacheMatrix, cacheSolve) are used to create a special object
## that stores a matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special “matrix”,
## which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function()
    x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function()
    inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}

## "cacheSolve" function computes the inverse of the special “matrix” returned by makeCacheMatrix function.
## If the inverse has already been calculated, then it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets the value of inverse in the cache using setinverse function.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("Getting the Matrix Inverse from Cached Data...")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}