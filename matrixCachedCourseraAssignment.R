## Caching the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(matrixSolution) inv <<- matrixSolution
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the cached matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Cached Data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

##a=matrix(8:11, nrow=2, ncol=2)
##b=makeCacheMatrix(a)
##c=cacheSolve(b)

##c

##Result will be matrix   
## -5.5    5
##  4.5   -4
