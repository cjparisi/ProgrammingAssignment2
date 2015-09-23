## The purpose of these functions is to generate an inverse of
## a matrix, and cache it so that subsequent requests for the
## inverse are not required to recalculate the matrix

## This function provides the environment to cache a matrix
## it provides the apis to set and store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will calculate the inverse of a matrix
## if the inverse is cached then it will return the cached value

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
