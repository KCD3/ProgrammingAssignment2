## Create a special "matrix" object that can cashe its inverse


## this function creates a matrix that contains a function 

makeCacheMatrix <- function(x = matrix(data = 1:4, nrow = 2, ncol = 2)) {
  m <- NULL
  set <- function(x) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- Solve
  getSolve <- function() m
  matrix(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}
## If the inverse has already been calculated, 
## then`cacheSolve` should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- Solve(data, ...)
  x$setSolve(m)
  m
}
## Return a matrix that is the inverse of 'x'