## Cache matrix
# This function define a list of sub-process :
# - On one hand set and get for matrix
# - On other hand set solve and get solve of matrix
makeCacheMatrix  <-  function(mymatrix = matrix())
{
  m <- NULL
  # set matrix's value
  set <- function(y) 
  {
    mymatrix <<- y
    m <<- NULL
  }
  #get matrix's value
  get <- function() mymatrix
  # set matrix's solve(inverse)
  setsolve <- function(solve) m <<- solve
  # get matrix's solve(inverse)
  getsolve <- function() m
  # return a list of sub-function
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Solve matrix
# This function compute inverse of matrix 
# wich is defined from cachematrix function

cacheSolve <- function(x, ...) 
{
  m <- x$getsolve()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


