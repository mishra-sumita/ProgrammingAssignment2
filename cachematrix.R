## Put comments here that give an overall description of what your
## functions do

## this function creates the matrix object that will cache the inverse

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




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## this function Return a matrix that is the inverse of 'x' it first checks 
  #whether it is available in cache memory 
  #and if it's there it will get it from cache otherwise it will calculate inverse.
  
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

