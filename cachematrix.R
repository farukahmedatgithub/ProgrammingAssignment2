## makeCacheMatrix() creates a special matrix object.
## It creates four methods set,get,setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL
  
  set <- function(y) {
  
    x <<- y
    
    inv_x <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    inv_x <<- inverse
  }
  
  getinverse <- function() {
    inv_x
  }
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, if
## not, it computes.

cacheSolve <- function(x, ...) {

  inv_x <- x$getinverse()
  
  if(!is.null(inv_x)) {
  
    message("pulling from cache.")
    
    return(inv_x)
  
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv_x)
  
  inv_x

}
