## compute and store the inverse of a matrix   

## create object with 4 functions that operate on inversible matrix
## list of functions:
## set (accepts input matrix); get (retrieves matrix);
## setinverse (accepts inverse of an input matrix);
## getinverse (outputs inverse of an input matrix)
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this function takes the output of makeCacheMatrix as an argument,
## retrieves inverse matrix if already present;
## if the inverse is not stored, then computes the inverse and
## stores it with setinverse()

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  message("calculating inverse")
  x$setinverse(inv)
  inv
}
