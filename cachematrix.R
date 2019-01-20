##Creates a special matrix object that can catch the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solMatrix) inv <<- solMatrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


## Creates the inverse of a matrix

cacheSolve <- function(x, ...) {
        ##Return the inverse of matrix x
  invers <- x$getInverse()
  if(!is.null(invers)){
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  invers <- solve(data)
  x$setInverse(invers)
  invers      
}
