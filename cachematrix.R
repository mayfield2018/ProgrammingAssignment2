## These are a pair of functions that cache the inverse of a matrix

###This function creates a special "matrix" object that can cache its inverse.It 
## doesn't operate number, only allcate the the space for "matrix"


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL            ## make sure m doesn't contain a value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }    ## make sure x and m doesn't contain a value 
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse   ## asign the inverse value to m
  getinverse <- function() m                      ## retrive the value of m
  list(set = set, get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("retrive cached data")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  
  x$setinverse(m)
  m
}