## Creates a matrix object that can cache its inverse
## I set the input 'inv' as the inverse;
## 'mtx' for matrix;
## and 'get' and 'set' for the inverse methods
## then I changed every reference to "mean" to "solve"


makeCacheMatrix <- function(m = matrix()) {
  
  ## We will Initialize the inverse property first
  inv <- NULL
  
  ## Set Method to the matrix
  set <- function(matrix) {
    mtx <<- matrix
    inv <<- NULL}
  
  ## This is the method to get the matrix
  get <- function() {
    mtx}
  
  ## Get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    inv}
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse}
  

  ## Return a list of the get-set inverse methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  mtx <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()

  ## Calculate the inverse
  mtx <- solve(data) %*% data
  
  ## Set the inverse to the object;
  x$setInverse(mtx)

  ## and we'll Return mtx
  mtx
}