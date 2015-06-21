## Desc: Funcitons to implement caching for matrix inverses 
## Usage: 
##      Inputs: 
##      > x <- matrix(c(1,0,0,1),nrow = 2,ncol = 2)
##      > x1 <- makeCacheMatrix(x)
##      > x2 <- cacheSolve(x1)
##      Output:
##      inverse matrix of x. retrieved from cache.  

## This function gets/sets up matrix and its inverse in env 
## for caching
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # set function for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get function for matrix
  get <- function() x
  #set inverse for the matrix
  setinv <- function(inv_m) inv <<- inv_m
  #get inverse 
  getinv <- function() inv
  #list returned with functions 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks if the inv exists to a cached matrix and returns
## else it computes and sets it in the env. for future use. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  ## check if data is available as cached 
  if(!is.null(m)) {
    message("getting cached data")
    m
  } else {
  message("Computing data")
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  }
}
