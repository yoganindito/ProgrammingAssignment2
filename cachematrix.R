## These functions combined, if used in a proper manner, can return an inverse of a given matrix and then keep the inversed matrix in a cache. If we want to do the same operation in the future, R can simply return the saved value without having to re-calculate the inverse.
## To use it, one must assign 'makeCacheMatrix(anyMatrix)' to a variable, then use that variable as an argument when calling the function cacheSolve().

## This is the cache assignment matrix. It will set the initial value of the matrix and its inverse (NULL) and store their value after the function that changes the matrix or calculates the matrix's inverse assign the new value. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {   ##For this assignment, this function is not called, but can be used if we want to change our matrix by writing another function
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This is the calculating function. When called, the function will assign the getinverse() function specified in makeCacheMatrix function to i. If i returns a value, then it implies that this operation has been done on the matrix. The function will return the stored value and give a notification about it. If no calculation has been previously done, it will calculate the inverse and store it in the argument variable's local environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
