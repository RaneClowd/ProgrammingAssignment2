## These functions work as a pair to manage the storage of the
##  inverse of a matrix and avoid re-calculating it if that's
##  avoidable.


## Can be used to create a wrapper object around a matrix with
##  the primary purpose being to hold the matrix and the inverse
##  of that matrix if the cacheSolve matrix has been used to
##  calculate it. The wrapper object, a list, also contains
##  functions to get and set the matrix and inverse stored within.

makeCacheMatrix <- function(dataMatrix = matrix()) {
  # Variable for storing the matrix's inverse
  inv = NULL
  
  # Getters and setters for the stored matrix
  setFunc = function(newVal) {
    dataMatrix <<- newVal
    inv <<- NULL # New matrix means old inverse isn't valid anymore
  }
  getFunc = function() dataMatrix
  
  # Getters and setters for the stored inverse
  setInverseFunc = function(inverse) inv <<- inverse
  getInverseFunc = function() inv
  
  # Return a list wrapper object giving access to the matrix
  #  and its inverse
  list(set = setFunc, get = getFunc,
       setInverse = setInverseFunc, getInverse = getInverseFunc)
}


## Used to retrieve or calculate the inverse of a matrix stored
##  within a wrapper list created by the makeCacheMatrix function.

cacheSolve <- function(matrixWrapper, ...) {
  #Retrieve the inverse stored in the wrapper object
  inverse = matrixWrapper$getInverse()
  
  if (!is.null(inverse)) {
    #Return the pre-calculated inverse
    message("getting cached data")
  } else {
    # No inverse was found, so calculate it
    message("calculating data")
    dataMatrix = matrixWrapper$get()
    inverse = solve(dataMatrix)
    
    # Store the caluclated inverse for future use
    matrixWrapper$setInverse(inverse)
  }
  
  inverse
}
