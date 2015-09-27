## makeCacheMatrix takes a square matrix as input and returns a list of functions
## get() - This function will return the input matrix. Will be used to read the matrix 
##         only when there is a new input and hence the inverse matrix is not available in the cache
## setinv() - This function is used to cache the Inverse computed by the function cacheSolve()
## getinv() - This function is used by cacheSolve() to get the cache'd Inverse matrix of the input 
##            especially when the function executes for the same input.

makeCacheMatrix <- function(x = matrix()) {

## Inverse Matrix "inv" that is stored in cache is reset with Default value of NULL when this function 
##       is executed which will mostly happen for new input Matrix

    inv <- NULL

## function definitions
  
    get <- function() x
    setinv <- function(invmat) inv <<- invmat
    getinv <- function() inv

    list(get = get, setinv = setinv, getinv = getinv )
}




## cacheSolve takes the list of functions ( returned by makeCacheMatrix) as input and execute these
##            functions in order to compute and return the inverse of the input matrix 
## This function computes Inverse only when it is not available in cache, that is the function "makeCacheMatrix" 
##            has been executed for a new input matrix

cacheSolve <- function(x, ...) {

    inv <- x$getinv()
   
    if (!is.null(inv)) 
       {
      	  message("getting cached data")
       	  return(inv)
       }

    message("not from cache")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)

    inv
}
