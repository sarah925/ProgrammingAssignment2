## A pair of functions that cache the inverse of a matrix. 
## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse 
## from the cache.


## makeCacheMatrix creates a special "matrix" object that which 
## is really a list containing a function to
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
  #setmean <- function(mean) m <<- mean
  #getmean <- function() m
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  cacheSolve calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
