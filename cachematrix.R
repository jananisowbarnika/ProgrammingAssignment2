## The functions defined below returns the inverse of a matrix with minimum re calculations.
## The 2 functions are makeCacheMatrix and cacheSolve.

## makeCacheMatrix takes a matrix as an argument and defines a special matrix with functions like set, get, setInverse and getInverse.

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matrix_inverse <<- inverse
  getInverse <- function() matrix_inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes any matrix as argument and returns if matrix has already been inversed and is present in cache. Otherwise solves for inverse and returns it.

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getInverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  mat <- x$get()
  matrix_inverse <- solve(mat, ...)
  x$setInverse(matrix_inverse)
  matrix_inverse
}
