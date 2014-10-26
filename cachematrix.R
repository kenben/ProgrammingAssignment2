## Simple cache functions for matrix inverses,
## consisting of a matrix object that calculates and
## stores the inverse, and a function to call the object.

## Special matrix object, providing functions to
## get and set the matrix, as well as get and set
# the matrix inverse. Assumes matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(y){
        x <<- y
        inverse <<- NULL # if the matrix is changed, 
                         # forget any previously calculated inverse
    }
    
    get <- function() x
    
    setInverse <- function(inv){
        inverse <<- inv
    }
    
    getInverse <- function() inverse
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function that checks if inverse of x has already 
## been calculated, returns previously calculated inverse
# if so, otherwise calculates the inverse.
# Note: The function only checks for reference identity of 
# the matrix object, NOT if the values in the matrix are the same.

cacheSolve <- function(x, ...) {
        
    # get the inverse from our convenience object
    inverse <- x$getInverse()
    
    # if the inverse hadn't been calculated, it's not NULL
    if(!is.null(inverse)){
        message("Getting cached inverse...")
        return(inverse) # return ends function execution, too
    }
    
    # if we make it here, the inverse is NULL, 
    # so we need to calculate
    
    inverse <- solve(x$get(),...)
    
    # store the result
    x$setInverse(inverse)
    
    # return the result
    inverse
    
}
