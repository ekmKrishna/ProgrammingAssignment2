## Put comments here that give an overall description of what your
## functions do

## This function will  create a specail matrix and other functions like setters to set teh imput Matrix 
## and getters to display the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvm <- function(InvM) m <<- InvM
  getInvm <- function() m
  list(set = set, get = get,
       setInvm = setInvm,
       getInvm = getInvm)
}

## This function will call the makeCacheMatrix , and will check whether the inverse matrix variable is null or not.
## if it is null , Inverse Matrix will be created. Inverse matrix will be created for the Matrix which are invertible.
## If the determinant of the Matrix is not 0 then only inverse can be compute. det(matrix) function can be used to know 
## the determinant of the matrix. 
## Assumption-- input matrix is always invertible as per the requirement


cacheSolve <- function(x, ...) {
  m <- x$getInvm()
  if(!is.null(m)) {
    message("getting cached Inverse Matrix")
    return(m)
  }
  data <- x$get()
  #  print(data)
    m <- solve(data, ...)
  x$setInvm(m)
  m
}
