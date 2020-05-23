## The following functions are used to retrive data (inverse of a matrix) from the cache memory if it is already
## available, else will calculate and return the values.

## This function takes the matrix that needs to be inverted and returns a list of 4 functions
## that get the matrix, set the matrix, get the inverse and set the inverse
## In this function we are setting a flag variable to NULL which indicates that the cache is empty

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## This function takes an arguement of a list that contains 4 functions set, get, setinv, and getinv.
## It first checks if the cache already holds the inverse of the matrix, and if so, returs it, else
## it calculates the inverse and returns it. When this function is called, the flag variable m is changed to
## value of the inverse of the matrix and will no more be null because the previous function gets called.

cacheSolve <- function(x, ...) {
        
  m <- x$getinv()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
