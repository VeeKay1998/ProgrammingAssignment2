
## set is used to set the matrix
## getMatrix is used to get matrix stored
## getInv is used to get the inverse of the matrix
## setInv sets the the computed inverse value to the inv


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  getInv <- function() inv
  setInv <- function(powerNeg1) inv <<- powerNeg1
  list(set = set, getMatrix = getMatrix, getInv = getInv, setInv = setInv)
}


## Here in this function,
## we check that if a matrix is  cached,
## if cached we display the cached value
## or else it computes the inverse...
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) { ## Checking for cached value
    message("GETTING CACHED DATA !!!")
    return(inv)
  } 
  data <- x$getMatrix()
  inv <- solve(data) ## Computes Inverse of the matrix!!!
  x$setInv(inv)
  inv
}

