#The functions cache the inverse matrix for a given matrix 
#(assuming that any matrix that goes in is invertable.)
#The function caches the inverse nad returns that matrix in case the original matrix didn't chnage
#or reclaculates new inverse matrix if the matrix have changed.

#The function makeCacheMatrix creates a special "vector", 
#which is a list containing a function to
# - set the value of the matrix for which inverse matrix is to be calculated
# - get the value of the matrix
# - set the inverse matrix
# - get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL #ini inv NULL matrix
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) minv <<- solve #gets inv matrix
  getsolve <- function() minv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#This function gets the results of the makeCacheMatrix and if inverse matrix is not null
#returns cached value.
#Otherwise, the function recalculates new inverse matrix.
cacheSolve <- function(x, ...) {
  minv <- x$getsolve()
  if(!is.null(minv)) {#if matirx didn't change return
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)#if matrix changed solve new
  x$setsolve(minv)
  minv
}