## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## Create a list that has 4 member functions:
## set, get, setInverse, getInverse. We will use
## the <<- assignment operator which can be used to 
## assign a value to an object in an environment that 
## is different from the current environment. This function
## makes sure that the internal variables are not exposed to
## outside environment
makeCacheMatrix <- function(x = matrix()) {
        # xinv will store the result of the matrix inversion
        xinv <- NULL                       #it will be null if inverse not calculated before
        # setter function to set a matrix to the object created by 
        # makeCacheMatrix function
        # makeCacheMatrix(test_matrix) test_matrix will be set to input matrix
        set <- function (y) {
                x <<- y
                xinv <<- NULL              #initializes xinv = null
        }
        #getter function to return the input matrix
        get <- function() x
        
        #settter and geeter functions to set and get the inversed matrix
        setInverse <- function (inv) xinv <<- inv
        getInverse <- function () xinv
        
        # return a list that contains the set and get functions,
        # so that we can use makeCacheMatrix object below
        # x <- makeCacheMatrix(test_matrix)
        # x$set(test_matrix) to change matrix
        # x$get to get the setted matrix
        # x$setInverse to set the inversed matrix
        # x$getInverse to get the inversed matrix        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getInverse()
        # Check if inverse of the matrix exists
        if(!is.null(inv_matrix)) {
                message("The inverse was cached, retrieving inverse from cache")
                return(inv_matrix)
        }
        # if inverse of the matrix doesn't exist, we do x$get to get matrix object
        input_matrix <- x$get()
        # we solve the matrix using solve function 
        inv_matrix <- solve(input_matrix)
        #we then set the inverse to the object
        x$setInverse(inv_matrix)
        
        #return the inverse matrix
        inv_matrix
}
