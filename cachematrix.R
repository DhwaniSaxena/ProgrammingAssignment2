## Put comments here that give an overall description of what your
## functions do

##Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.
##(1)get is a function that returns the vector x stored in the main function.
##(2)set is a function that changes the vector stored in the main function.
##(3)setinverse and getinverse are functions very similar to set and get.They store the values of inverse

## Write a short comment describing this function

## This function creates a special matrix object that can cache its inverse.

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

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

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

## Taking a matrix
z <- diag(2,4)

## Caching the Matrix
CacheMatrix <-makeCacheMatrix(z)

## Inverse of Matrix
cacheSolve(CacheMatrix)
