## These two functions are used to save time and computational resources by using cached values if possible.
## This is really useful if you are running a computation (like mean) on a long vector, or if it is done repeatedly in a loop.
## Using cached values speeds up the process if the contecnts of the vector aren't changing.

## This function sets the value of this command, which is a matrix rather than a vector. 
## I then ask it to get the value of that matrix using 'solve' to replace the 'mean' given in the example.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function looks to see if the mean has already been calculated in the first function.
## If it is has it doesn't calculate it again but uses the cached value instead.
## If it hasn't been done before, it calculates the mean and adds the value to the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
