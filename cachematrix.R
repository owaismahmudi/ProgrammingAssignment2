## Put comments here that give an overall description of what your
## functions do

## The following function, makeVector creates a special "matrix", which is 
## used to
##  1  set the value of the matrix
##  2  get the value of the matrix
##  3  set the value of the inverse of matrix
##  4  get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinversematrix <- function(imatrix) ix <<- imatrix
  getinversematrix <- function() ix
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the setinversematrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getinversematrix()
        if(!is.null(ix)){
          message("getting cached data")
          return(ix)
        }
        data <- x$get()
        ix <- solve(data)
        x$setinversematrix(ix)
        ix
}
