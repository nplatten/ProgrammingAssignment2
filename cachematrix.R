## Pair of functions which create an object which stores a matrix and 
## the matrix's inverse. If the inverse has not been previously calculated,
## the function caches the inverse. If the inverse has been previously 
## calculated, the function returns the cached inverse, instead of 
## recalculating the inverse.
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        #Set inverse variable to null
        inv <- NULL
        
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #Set get variable for list, to matrix value
        get <- function() x
        
        #Set setInv variable to inverse of matrix
        setInv <- function(solve) inv <<- solve
        
        #Set getInv variable to inverse variable value
        getInv <- function() inv
        
        # Create special object which caches matrix inverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Retrieve value of inverse matrix from cache
        inv <- x$getInv()
        
        ## If cache is not null (i.e. calculated before), then return cache value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Retrieve original matrix
        data <- x$get()
        
        ## Calculate inverse of original matrix
        inv <- solve(data, ...)
        
        ## Store value of inverse matrix 
        x$setInv(inv)
        
        ## Return inverse matrix
        inv
}
