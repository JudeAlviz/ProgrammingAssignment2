## for caching and getting the inverse of a matrix


## function can create matrix and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## get the inverse of the matrix from the function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
