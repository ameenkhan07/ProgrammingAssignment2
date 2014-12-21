## This function (makeCacheVector()) creates a special "matrix" object ,'x',
## and another matrix object ,'m',that can cache its inverse.
## RERTURNS a list containing each of the following function to :
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the inverse vector
## 4.get the value of the inverse vector

makeCacheMatrix <- function(x = numeric()) {        
        # holds the cached value or NULL if nothing is cached initially
        # (latter in its local environmnet) 
        m <- NULL
        
        # store/cache a matrix ,'x', 
        # if matrix is changed boot matrix vector containing inverse if x, 'm' to hold NULL
        setMatrix <- function(newValue) {
                x <<- newValue
                m <<- NULL
        }
        # returns the stored matrix
        getMatrix <- function() x

        # inverse the given argument and store/cache in matrix vector 'm'
        cacheInverse <- function(solve) m <<- solve

        # get the cached value
        getInverse <- function()  m
        
        # return a list. 
        # Each named element of the following list is a function which holds above computed/cached values
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             cacheInverse = cacheInverse, getInverse = getInverse)
}


## The following function (cacheSolve()) calculates the inverse of a "special" matrix created with 
## makeCacheMatrix if not already cached and returns it.
## Else returns the cached inverse matrix of said "special"matrix.

cacheSolve <- function(y, ...) {
        # get the cached value
        inv <- y$getInverse()
        # if a cached value exists return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # otherwise get the matrix, caclulate the inverse and store it in the cache
        data <- y$getMatrix()
        inv <- solve(data) #calcuate inverse
        y$cacheInverse(inv) #storing the inverse in the cache
        
        # return the inverse
        inv
}
