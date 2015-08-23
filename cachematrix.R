## Pair of functions that together allow you to create a matrix that will 
## efficiently cache its inverse rather than computing it each time. The 
## assumption is that the matrix is a square invertible matrix, so the 
## inverse is calculated using solve(x) where x is the matrix.

## Creates a 'matrix' (really a list) that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setminv <- function(inv) minv <<- inv
  getminv <- function() minv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## Either retrieves the matrix inverse from the cache, or computes it 
## if it has not yet been computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getminv()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  squareMat <- x$get()
  inv <- solve(squareMat)
  x$setminv(inv)
  inv
}