## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  Create the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  # set the  inverse to NULL
  inv <- NULL
  # Defines a function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##  Returns the matrix x
  get <- function() x
  ##  Sets the value of setinv to the inverse using the solve() function
  setinv <- function(solve) inv <<- solve
  ##  Returns the inverse 
  getinv <- function() inv
  ##  Returns our "special" matrix 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Create the CacheSolve function 
cacheSolve <- function(x, ...) {
  ##  Get the inverse of the matrix
  inv <- x$getinv()
  ##  If the value is null it needs to be calculated otherwise it will use the cached value
  if(!is.null(inv)) {
    ##  display the "getting cached data" message indicating that a cached value will be used
    message("getting cached data")
    return(inv)
  }
  ##  Obtain the underlying matrix
  data <- x$get()
  ## calculate the inverse of the underlying matrix
  inv <- solve(data, ...)
  ## set the inverse in x
  x$setinv(inv)
  ##  Retrun the inverse
  inv
}

##  Example execution
##  Create matrix
##  blorg <- matrix (c(1,0,5,2,1,6,3,5,0), nrow = 3, ncol = 3) 
##  Create variable "myMatrix" that uses makeCacheMatrix()
##  myMatrix <- makeCacheMatrix(blorg)

##  cacheSolve(myMatrix)



