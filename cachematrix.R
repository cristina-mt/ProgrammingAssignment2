## The set of functions solve the inverse of a matrix,
## and whenever the inverse already exists, it gets it
## from the cache. It assumes that the matrix is invertible!

## This function creates a special 'matrix' object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special 'matrix'
## returned by the above function. If the inverse has already
## been calculated then it should retrieve it from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message('getting chached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
