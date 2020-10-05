## Assignment: Caching the Inverse of a Matrix
##  USAGE:
##  > matr <- makeCacheMatrix()
##  > matr$set(matrix(c(4,2,7,6), nrow=2))
##  > matr$get()
##        [,1] [,2]
##  [1,]    4    7
##  [2,]    2    6
##  > cacheSolve(matr)
##        [,1] [,2]
##  [1,]  0.6 -0.7
##  [2,] -0.2  0.4
##  > cacheSolve(matr)
##  getting cached data
##       [,1] [,2]
##  [1,]  0.6 -0.7
##  [2,] -0.2  0.4
##  
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" or retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
