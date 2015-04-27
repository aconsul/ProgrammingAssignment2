## This file contains 2 funcions:
## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve will compute an inverse of a matrix returned by makeCacheInverse if it alread does not exist, else it will return from cache

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       
        invMatrix = NULL
        set = function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get = function() x
        setinvMatrix = function(inverse) invMatrix <<- inverse 
        getinvMatrix = function() invMatrix
        list(set=set, get=get, setinvMatrix=setinvMatrix, getinvMatrix=getinvMatrix)
}


## If the inverse has already been calculated, this function returns the inverse from cache
## Else, the inverse is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(invMatrix)){
                # get it from the cache and skips the computation. 
                message("getting matrix from cache")
                return(invMatrix)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        invMatrix = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinvMatrix(invMatrix)
        
        return(invMatrix)
}
