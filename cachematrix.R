## The following functions calculate the inverse of a given matrix and are 
## able to retrieve the inverse from cache if the inverse has been calculted 

## Creates an R object, makeCacheMatrix(), to store a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      # Set a matrix to be assigned to x
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      # Store an inverse value if it has been calculated by cacheSolve()
      setInverse <- function(inverse) I <<- inverse
      getInverse <- function() I
      
      # makeCacheMatrix() returns a list of four functions
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Calculate the inverse or Retrieve the inverse from the cached 
## that is stored in the makeVector() object's environment 
cacheSolve <- function(x, ...) {
      # Test if inverse of the given matrix has been calculated
      # If !I == NULL, cacheSolve() will retrieve the inverse
      I <- x$getInverse()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      # If I == NULL, cacheSolve() will calculate the inverse 
      data <- x$get()
      I <- inv(data)
      x$setInverse(I) 
      # cacheSolve() returns value of the inverse
      I
}
