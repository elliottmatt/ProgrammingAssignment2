# Matt Elliott (elliottmatt)
# 2015/01/20
# https://class.coursera.org/rprog-010/
# R Programming
# by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD

# inversing a matrix can be an expensive operation
# use these functions to create an object that caches the inverse

# example usage:
# > c <- rbind(c(1, -1/4), c(-1/4, 1))
# > m <- makeCacheMatrix(c)
# > cacheSolve(m)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > cacheSolve(m)
# getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667


## Input:  square matrix (assumes it is inverse-able)
## Output: m object with 4 functions
##         m is not useful alone and should be used with cacheSolve()
##
## This function sets up the matrix in a wrapper that will
## cache the result once it is calculated once.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Input:  x must be the output of the 'makeCacheMatrix'
## Output: matrix that is the inverse of the input
##
## This function will calculate the inverse matrix and store it
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
