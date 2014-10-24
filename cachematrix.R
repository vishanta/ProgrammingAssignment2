## Creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix function returns a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize a variable 'm' and set it to NULL
  m <- NULL

  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ## Method to get the matrix
  get <- function() {
    x
  }

  ## Method to set the inverse of the matrix and store the value in a cache
  setInverse <- function(inverse) {
    m <<- inverse
  }

  ## Method to get the inverse of the matrix which is a cached value
  getInverse <- function() {
    m
  }

  ## Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
## Function cacheSolve computes the inverse of a "special" matrix created with makeCacheMatrix
## Returns inverse from the cache if the inverse is already computed and not changed

cacheSolve <- function(x, ...) {
  ## Get the cache data
  m <- x$getInverse()

  ## Return cached data if available
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  ## Get the matrix if cache data does not exist
  data <- x$get()

  ## calculate the inverse of the matrix
  m <- solve(data, ...)

  ## Set the inverse of the matrix to cache by calling parent function
  x$setInverse(m)

  ## Return the matrix
  m
}
