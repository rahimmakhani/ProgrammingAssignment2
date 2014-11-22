## The script is used to compute the inverse of a matrix.
## The inverse is cached in memory to avoid any recomputation if 
## the matrix is needed again.


## This function creates a special 'matrix' that has functions to
## set the values of the matrix
## get the values of the matrix
## set the inverse of the matrix
## get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The function caches the inverse of the matrix if it is already computed
## Otherwise the function computes the inverse of a matrix.

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
