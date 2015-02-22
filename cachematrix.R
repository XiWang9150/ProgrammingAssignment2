## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # set up the initialized matrix
  mx <- NULL
  set <- function(y){
    x <<- y
    mx <<- NULL
  }
  
  #get value for the matrix
  get <- function() x
  #set the value of the inverse
  setinverse <- function(inverse) mx <<- inverse
  #get value of the inverse
  getinverse <- function() mx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
    )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mx <- x$getinverse()
  #check the value of inverse. If exists, then return the value
  if(!is.null(mx)){
    message("getting cached data")
    return(mx)
  }
  
  #if not existing, read the matrix and compute the inverse
  data <- x$get()
  #Use Solve function to compute inverse
  mx <- solve(data,...)
  x$setinverse(mx)
  mx
}
