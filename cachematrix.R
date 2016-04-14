##makeCacheMatrix: This function creates a special 
##"matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  ##set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  ##get the inverse of the matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve: This function computes the inverse 
##of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##Example:
##g <- matrix(1:4,2,2)
##g
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##CachedMarix <- makeCacheMatrix(g)
##cacheSolve(CachedMarix)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5