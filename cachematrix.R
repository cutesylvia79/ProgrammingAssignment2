## Put comments here that give an overall description of what your
## functions do

## Cache the inverse of matrix using concept lexical scooping
## Recalculate can be based on the cache copy rather than having to calculate the inverse of matrix again

## Write a short comment describing this function
## this function will create the special matrix and store it as cache copy

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
##This function calculate the inverse of the special "matrix" returned by makeCacheMatrix function. 
##If the inverse has already been cached and calculated (and the matrix stayed the same), 
##then the cachesolve function should retrieve the inverse value from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
