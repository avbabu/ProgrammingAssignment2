#makeCacheMatrix function creates a list with four functions
# function-1: setMatrix to set the value to the matrix
# function-2: getMatrix to get the value from the matrix 
# function-3: setInvMatrix to set the value of the inverse Matrix
# function-4: getInvMatrix to get the value from the inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInvMatrix <- function(sol) invMatrix <<- sol
  getInvMatrix <- function() invMatrix
  list (setMatrix= setMatrix, getMatrix = getMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}
          
#CacheSolve fucntion returns the inverse of the matrix.
#please provide Invertible matrix as an input to this function
cacheSolve <- function(x= matrix(), ...) {
  invMatrix <- x$getInvMatrix()
  if(!is.null(invMatrix)) {
    message("getting from cached data")
    return(invMatrix)
  }
  Mat <- x$getMatrix()
  invMatrix <- solve(Mat)
  message("Not getting from cached data")
  x$setInvMatrix(invMatrix)
  invMatrix
}
          
