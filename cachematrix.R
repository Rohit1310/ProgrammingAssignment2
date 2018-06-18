## Caching the inverse of a matrix rather than compute it repeatedly,
## saves a fair amount of time
## The following two functions are used to cache the inverse of a matrix.

## This function is to get,set the matrix and inverse of the matrix 
## respectively and retruns a list.

makeCacheMatrix <- function(x = matrix()) {

  m_inverse  <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m_inverse <<- inverse
  getinverse <- function() m_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## Tis function check if there is invere in the cache if not it will process
## the matrix to caculate the inverse of the given matrix.
## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  
  m_inverse <- x$getinverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- x$get()
  m_inverse <- solve(data)
  x$setinverse(m_inverse)
  m_inverse
       
}
