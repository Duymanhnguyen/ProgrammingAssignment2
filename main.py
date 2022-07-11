# Programming Assignment 2 in R Programming Course on Coursera
# R Language
makeCacheMatrix = function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

CacheSolve = function(x,...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Retrieve the reverse of the matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}