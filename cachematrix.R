## cachematrix.R
##
## Compute matrix inverse with caching

## Creates special matrix that automatically caches computed result.
##
## Field variables
##   x:     target matrix
##   func:  a function that is used for computation
##   cache: cache of result computed by `func(x)`
## Member functions
##   get:     Get the original matrix
##   set:     Overwrite original matrix
##   setFunc: Set a function that is used from `compute`
##   compute: Compute `func(x)` caching the result to `cache`
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  func <- NULL
  
  get <- function() x
  set <- function(newMatrix) {
    x     <<- newMatrix
    cache <<- NULL
  } 
  
  setFunc <- function(newFunc) {
    func  <<- newFunc
    cache <<- NULL
  }
  
  compute <- function() {
    if (is.null(func)) {
      message('fatal: func is not set')
      return()
    }
    if (!is.null(cache)) {
      message('Using result remaining its cache')
      return(cache)
    }
    
    cache <<- func(x)
    cache
  }
  
  list(get=get, 
       set=set, 
       compute=compute,
       setFunc=setFunc)
}


## Compute inverse matrix of given matrix created by `makeCacheMatrix`
cacheSolve <- function(x, ...) {
  x$setFunc(solve)
  x$compute()
}
