## Create method of storing matrices and calculating their inversions that only
## calculates the inversion for a given matrix once.

## Cached matrix data type (functions to store/retrieve matrix/inversion).
makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculate inversion of matrix stored in cached matrix data type if needed.
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x' (calculate if necessary).
  i <- x$getinverse()
  if(!is.null(i)) {
    message('getting cached data')
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
