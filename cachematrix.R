## These functions together provide for an efficient mechanism to invert
## a square invertible matrix - if the inverse of that matrix has to be
## accessed multiple times.  To do so, the input matrix is inverted the
## first time it is accessed, the inverted matrix is cached and the 
## inverted matrix is returned to the caller of cacheSolve().
## Upon subsequent calls to cacheSolve(), the cached inverted matrix
## is returned to the caller of cacheSolve().

## To use these functions, first call makeCacheMatrix() with the 
## original data matrix as input (this must be square and invertible).  
## Then, call cacheSolve() with the value returned by makeCacheMatrix()
## as many times as needed to access the inverted matrix.
## The second and subsequent calls to cacheSolve (for the same input object)
## will result in this message being printed: "getting cached data".


## The function 'makeCacheMatrix' is used to create (and return) 
## an object that provides a set of functions to:
##  1) set a data matrix to be inverted (set)
##  2) get the data matrix (get)
##  3) set the inverse of the data matrix (set_inverse)
##  4) get the (cached) inverse of the data matrix (get_inverse)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # initialize cached inverted matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverted_matrix) inverse <<- inverted_matrix
    get_inverse <- function() inverse
    list(set = set, get = get, 
         setinverse = set_inverse, getinverse = get_inverse)
}

## The function 'cacheSolve' takes as input the output of 
## makeCacheMatrix().  It then returns either the cached, inverted value
## of the original matrix (if available), or, otherwise, computes the
## inverted matrix, caches that value, and returns the inverted matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse = x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix = x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
