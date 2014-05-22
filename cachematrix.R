## This pair of functions will calculate and return the inverse
## of an invertible matrix. As matrix inversion is a costly operation, 
## this pair of functions takes advantage of caching results for 
## subsequent retrieval.

## makeCacheMatrix() takes a square invertible matrix as it's only argument.
## The <<- operator is used to assign a value to an object in an environment
## that is different from the current environment.

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

## cacheSolve() takes as it's argument a list object created by calling makeCacheMatrix().
## The function calculates the inverse of the matrix created in makeCacheMatrix().
## The member function getsolve() is called to check if the matrix inverse has already 
## been calculated and retrieves the result.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
