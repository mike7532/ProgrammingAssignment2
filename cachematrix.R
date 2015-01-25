## This file contains two functions: makeCacheMatrix, and cacheSolve.
## These two functions work together to return the inverse of an invertible 
## matrix.  The inverse is either calculated, or retrieved from cache if it has
## been previous calculated.

## The most appropriate way to call these functions is 
## cacheSolve(makeCacheMatrix(m)), where m is an invertible matrix.


###### Function makeCacheMatrix #################
## This function creates takes a matrix object m and creates a list of functions
## that will allow the cacheSolve function below to return the cached inverse of 
## m if the inverse has already been calculated, or calculate and return the 
## inverse if the inverse was not already calculated.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL                # set value of variable to hold inverse to null
   set <- function (y) {      # define function to set value of matrix
      x <<- y                 # in parent environment so it will be available to 
      inv <<- NULL            # next function
   }  
   get <- function() x        # define function to get value of matrix
   setsolve <- function(solve) inv <<- solve       # define function invert matrix
   getsolve <- function() inv                      # define function to 
   list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
                              # line above creates list of all functions defined above for use in next function
} # end function makeCacheMatrix

########## Function cacheSolve ###################
## this function uses the makeCacheMatrix list of functions returned
## by the makeCacheMatrix function above to return the inverse of
## the matrix "x" passed through the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
   inv <- x$getsolve()              
   if(!is.null(inv)) {              # check if inverse variable has value in global env  
      message ("getting cached inverse")   # if it does, returns current value
      return(inv)
   } # end if
   data <- x$get()
   inv <- solve(data,...)           # if inverse value not cached, calculates it
   x$setsolve(inv)
   inv
} # end of function cacheSolve