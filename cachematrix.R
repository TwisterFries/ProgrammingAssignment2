## makeCacheMatrix stores the matrix whose inverse is to be computed
## and its inverse
## cacheSolve computes the inverse of the matrix and stores it in the
## cache

## makeCacheMatrix creates a list consisting of functions that
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
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


## cacheSolve take the list created by makeCacheMatrix as input and
## checks if it has not been computed already (m is not empty)

## if m is empty, the inverse is computed and stored into m

## if m is not empty, cacheSolve simply returns m and skips computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
