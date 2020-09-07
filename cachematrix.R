## Two short R functions that:
## 1) store a square matrix and it's inverse (makeCacheMatrix), 
## and 
## 2) compute and return the matrix inverse as well as store the matrix inverse 
## for future use (cacheSolve).

## The function makeCacheMatrix stores a square matrix in the local variable x and cache's
## it's inverse in the local variable m. The square matrix x is it's only formal argument.
## The local variables are x and the matrix inverse, m.
## The setter and getter functions of makeCacheMatrix 
## are: set, get, setmean, getmean
## The return values of these four functions are returned as elements of a list.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve accesses and returns the matrix inverse of the makeCacheMatrix 
## object passed to it.
## It's formal arguments are the makeCacheMatrix object, x, and ... arguments to be passed to
## the solve() function.
## It's local variables are the makeCacheMatrix object, x, and the matrix inverse, m.
## The return variable is the matrix inverse, m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
