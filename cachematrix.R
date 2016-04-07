## This function returns a list of functions that are responsible for storing and retrieving the matrix and it's inverse. This function caches/stores the computed 
inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setNewMatrix <- function(newMatrix){
    x <<- newMatrix
    inv <<-NULL
  }
  getMatrix <- function() {x}
  setInverse <- function(i) {inv <<- i}
  getInverse <- function() {inv}
  list(set=setNewMatrix,get=getMatrix,setInverse=setInverse,getInverse=getInverse)
}


## This functions retrieves matrix inverse if it is already available (cached), else, computes and stores it in cache and returns the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  return(inv)
}
