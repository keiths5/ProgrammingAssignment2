## This source file of functions takes a square matrix and will provide the inverse of the square matrix
##  because the inverse of a square matirx is a compute intensive operation, a cache of the inverse is stored.
##  If the inverse has already been computed, then the cached inverse is returned instead of re-computing the inverse.

##This function takes in a matrix and returns a list of functions available, the are:
##  set - creates the matrix, and sets im (inverse matrix) to NULL
##  get - retruns the original matrix
##  setinverse - should only be called by cacheSolve
##  getinverse - returns the invere matrix
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  
  ##This function sets the value of the matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  get <- function() x                              ##retruns the matrix
  setinverse <- function(inverse) im <<- inverse   ##should not be called except by cacheSolve, sets the value of the inverse matrix
  getinverse <- function() im                      ##gets the inverse matrix - if cacheSolve hasn't been called, this is NULL
  list(set = set, get = get,                       ##the list of available functions
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function takes in a makeCacheMatrix and returns the inverse matrix (im).  Because this is a compute intensive operation,
##  caching is utilized so as not to re-compute a inverse matrix that is already available in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {                                  ##checks to see if a cached inverse matrix is available
    message("getting cached inverse inverse matrix")  ##helpful message...
    return(im)                                        ##return out of the function - don't solve the matrix
  }
                                                      ##a cached inverse matrix not found
  data <- x$get()                                     ##get the data of the matrix
  im <- solve(data, ...)                              ##solve the inverse matrix
  x$setinverse(im)                                    ##set the inverse matrix in makeCacheMatrix so it can more easily be returned
  im                                                  ##return the inverse matrix
}