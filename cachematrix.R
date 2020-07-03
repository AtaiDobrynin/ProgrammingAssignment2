## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function creates cache matrix with getter and setter functions. 

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  matr <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_m <<- inv
  getinv <- function() inv_m
  list(matr = matr, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve function checks the presence of inverse matrix
## in cache matrix. If the inverse has already been calculated 
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached inverse matrix")
    return(inv_m)
  }
  matr <- x$get()
  inv_m <- solve(matr)
  x$setinv(inv_m)
  inv_m
        ## Return a matrix that is the inverse of 'x'
}
