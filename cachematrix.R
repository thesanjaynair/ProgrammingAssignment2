## Functions "makeCacheMatrix" and "cacheSolve" cache the inverse of a matrix
## Here, function "makeCacheMatrix" creates a matrix that caches its inverse (we assume that the given matrix is invertible)


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Here, we write function "cacheSolve" that computes the inverse of the matrix that's returned by "makeCacheMatrix".
## If the inverse is already calculated without any changes being made to the matrix, then, "cacheSolve" returns inverse from the cache.


cacheSolve <- function(x, ...) {
  
  ## Returns inverted matrix of x
  
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat,...)
  x$setinverse(inv)
  inv
}

cacheSolve(m1)