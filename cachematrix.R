## Put comments here that give an overall description of what your


## Write a short comment describing this function
## Create a "special" matrix


y <- matrix(sample(10,100,T),10)

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
      mtx <<- x;
      inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
  }
## Write a short comment describing this function
## Caching the inverse of "special" matrix

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}

fc1 <- makeCacheMatrix
fc2 <- cacheSolve

fc1(y)
fc2(y)

