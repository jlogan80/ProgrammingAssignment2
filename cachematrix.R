makeCacheMatrix <- function(x = matrix()) {
  #this function attempts to calculate the inverse of the matrix "x" and store the inverse
  # what should print out is a list containing the results of 4 functions:
  #              1. this one sets the matrix ("set")
  #              2. this one gets the matrix ("get")
  #              3. this one calculates the inverse ("setinverse")
  #              4. this one retrieves the inverse ("getinverse")
  #this list is important because it is used as the input to the next function, cacheSolve()
  inv = NULL
  set = function(y) {
    # "<<-" assigns a value to an object in the parent environment 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  #here, x is the output of makeCacheMatrix()
  #this function should output the inverse of the original matrix x given to makeCacheMatrix()
  
  inv = x$getinv()
  
  #if the inverse has already been calculated
  if (!is.null(inv)){
    # if the inverse has already been calculated, retrieve the matrix from the cache instead of computing again 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculate the inverse 
  mydata = x$get()
  inv = solve(mydata, ...)
  
  # sets the value of the inverse in the cache via the setinv function
  x$setinv(inv)
  
  return(inv)
}