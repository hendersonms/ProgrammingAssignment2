## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  Create the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Create the CacheSolve function 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

##  Example execution
##  Create matrix
##  blorg <- matrix (c(1,0,5,2,1,6,3,5,0), nrow = 3, ncol = 3) 
##  Create variable "myMatrix" that uses makeCacheMatrix()
##  myMatrix <- makeCacheMatrix(blorg)

##  cacheSolve(myMatrix)



