## These two functions create a special object with initial matrix (makeCacheMatrix) and caches its inverse matrix (cacheSolve)

makeCacheMatrix <- function(x = matrix()) {  ## Creates special "matrix" (list of functions)
  m <- NULL
  set <- function(y) {   ## set the value of the matrix 
    x <<- y
    m <<- NULL
  }
  get <- function() x   ## get the value of the matrix
  setsolve <- function(solve) m <<- solve  ##set the value of the inverse matrix
  getsolve <- function() m   ## get the value of the inverse matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  ## returnes a list of functions described above
}


cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)   ## return matrix if it was inversed before
  }
  data <- x$get()
  m <- solve(data, ...) 
  x$setsolve(m)
  m     
}
