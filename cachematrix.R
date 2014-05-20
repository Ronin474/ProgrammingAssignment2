# function makeCacheMatrix() creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # "local variable" of special "matrix" object that stores value of its inverse matrix
    # during initialization, inv is set to NULL
    inv <- NULL
    
    # "local function" that sets the value of the special "matrix" object
    # and also sets inv to NULL
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # "local function" that gets the value of the special "matrix" object
    getMatrix <- function()
        x
    
    # "local function" that sets the value of inverse of the special "matrix" object
    # and stores it in the "local variable" inv
    setInverse <- function(inverse)
        inv <<- inverse
    
    # "local function" that gets the value of the inverse matrix stored in the special "local variable" inv
    getInverse <- function()
        inv
    
    # return value of function makeCacheMatrix()
    # list of "local functions", similar to class methods of the OO programming paradigm
    # this list of functions allows easier manipulation of the special "matrix" object
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}



# function cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
    # getting the inverse matrix value from the special "matrix" object with its getInverse() function
    # if inverse matrix has not yet been calculated, variable i is equal to NULL
    i <- x$getInverse()
    
    # if loop that checks if there is a cached inverse matrix already stored within
    # the special "matrix" object, i.e. stored in its "local variable" inv
    if(!is.null(i)) {
        # notification message
        message("Getting cached matrix!")
        # returning cached inverse matrix if avaiable, i.e. if not still set to NULL
        return(i)
    }
    
    # getting the matrix value (stored in "local variable" x)
    # from the special "matrix" object with its "local function" getMatrix()
    data <- x$getMatrix()
    
    # calculating the inverse matrix value using function solve() available in base package
    i <- solve(data, ...)
    
    # storing the inverse matrix value in the special "matrix" object
    x$setInverse(i)
    
    # return value of function cacheSolve()
    # this inverse matrix value is returned only if it has not already been cached
    # in the "local variable" inv of the special "matrix" object
    i       
}
