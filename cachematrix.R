## This functionn takes as input a matrix, preferably an invertible matrix.
## Then it returns a list of functions, which does the following tasks.
##1. Get the matrix, 2. set the matrix, 3. Gets the inverse of matrix, 4. sets the inverse of matrix.


makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The below function calculates the inverse of the matrix created in the above function.
## the function first checks if the matrix and its inverse has already been calculated, in that case
## it just returns the already existing matrices stored in the cache otherwise it calulates a fresh one.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  #Condition to check if inverse fucntion has already been calculated.
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  x$set(data)
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
