
## This file has two main functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix is a function to create a matrix object 
## that also has four functions associated with it:
##   set: allows value of matrix to be set (starts as x when makeCacheMatrix called)
##   get: returns current value of matrix
##   setinv: allows value of inverse to be set (starts as NULL)
##   getinv: returns current value of inverse


## cacheSolve returns the inverse of a matrix object 
## if there is a non-NULL stored inverse, it gives that stored value
## otherwise it computes the matrix inverse of the stored matrix, 
##              stores it as inverse, and returns that.


makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL ##start with no inverse stored
    
    set <- function(y) {  ## puts new y as our matrix
        x <<- y  ## superassigns new matrix (so it's not just within the set function)
        inv <<- NULL  ## clears stored inverse if set is used
    }
    
    get <- function() { x } ## gives current matrix
    
    setinv <- function(inverse) { inv <<- inverse } ## superassisngs inverse
    
    getinv <- function() { inv } ## gives stored inverse
    
    ## this last bit means that the four functions are kept:
    list(set = set, get = get, setinv = setinv, getinv = getinv) 
}



cacheSolve <- function(x, ...) {
      
    inv <- x$getinv() ## tries to pull in stored inverse (could be NULL)
    
    ##tell the user it is using cached inverse, if not null 
    if(!is.null(inv)) { 
        message("getting cached inverse") 
        return(inv)
    }
    
    ## continues to here if cached inverse was not there
    ## so need to compute the inverse and store it
    
    data <- x$get()   ## get the matrix
    
    inv <- solve(data, ...)   ## invert the matrix (assumed invertible!!)
    
    x$setinv(inv) ## store our new inverse 
    
    inv  ## output the new inverse
}
