

makeCacheMatrix <- function(x = matrix()) {
  
  iv <- NULL
  
  
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  
  
  get <- function() x
  
  
  setinverse <- function(v) iv <<- v
  
  
  getinverse <- function() iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
  iv <- x$getinverse()
  
  
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  
  
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv
}
