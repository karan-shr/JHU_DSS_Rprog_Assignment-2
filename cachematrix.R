## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix accepts a matrix and makes a cached matrix from this matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x   #gets the current matrix from parent env.
  setinv <- function(solve) m <<- solve  # var. m from parent env. gets value of solve
  getinv <- function() m  #gets the value of m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # makeCacheMatrix(anyMatrix) results in a list of these 4 functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()  #if inverse is not calculated, NULL is returned
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() #data is assigned matrix x 
  m <- solve(data, ...) # inv. is calculated and stored in m
  x$setinv(m)  #value stored in the 'setinv' list func. of the cached Matrix
  m
}
