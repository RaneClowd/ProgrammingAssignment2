## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(dataMatrix = matrix()) {
  inv = NULL
  setFunc = function(newVal) {
    dataMatrix <<- newVal
    inv <<- NULL
  }
  getFunc = function() dataMatrix
  setInverseFunc = function(inverse) inv <<- inverse
  getInverseFunc = function() inv
  list(set = setFunc, get = getFunc,
       setInverse = setInverseFunc, getInverse = getInverseFunc)
}


## Write a short comment describing this function

cacheSolve <- function(matrixWrapper, ...) {
  inverse = matrixWrapper$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  message("calculating data")
  dataMatrix = matrixWrapper$get()
  inverse = solve(dataMatrix)
  matrixWrapper$setInverse(inverse)
  inverse
}
