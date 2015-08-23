## makeCacheMatrix checks whether the input is a square matrix, then creates 
## a vector of functions which set/get the value of the input, and set/get
## the inverse of the input matrix.
## cacheSolve checks to see if the inverse of the input matrix has already 
## been calculated. If so, it retrieves it from cache. If not, it calculates 
## it and caches it.

## makeCacheMatrix: first, check if the input is a square matrix; if it isn't,
## break and print an error message. If the input is a square matrix, create a
## list of four functions that set/get the input, and set/get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  if (!is.matrix(x)) {
    print("Input is not a matrix. Terminate.")
    break
  } else if (dim(x)[1]!=dim(x)[2]) {
    print("Input is not a square matrix. Terminate.")
    break
  }
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


## CacheSolve: first, check if the inverse has already been calculated; if so,
## return it; if not, calculate it and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
