
################################################################################
## File: cachematrix.R
## Author: César Pinto. Chile
## Original Source: Forked from https://github.com/rdpeng/ProgrammingAssignment2
################################################################################
## DESCRIPTION
##
##The following functions (makeCacheMatrix & cacheSolve) allow to cache the 
##inverse of a matrix rather than compute it repeatedly 
#################################################################################


## Function: makecacheMAtrix
## Description: Creates a special "matrix" object that can cache its value and inverse
## Parameter: x matrix()


makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        
        ##Defining set() function
        ##Description: set the value of a matrix. Initializing Inverse to NULL
        ##Parameter: y matrix()
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }

        ##Defining get() function
        ##Description: get the value of a matrix.
        ##Parameter: NONE
        get <- function() x

        ##Defining set_inv() function
        ##Description: set and caching the Inverse of a matrix. 
        ##Parameter: inv matrix()
        set_inv <- function(inv) m_inv <<- inv
        
        ##Defining get_inv() function
        ##Description: get the Inverse of a matrix.
        ##Parameter: NONE
        get_inv <- function() m_inv
        
        
        list(set = set, 
             get = get,
             set_inv = set_inv,
             get_inv = get_inv)
        
}


## Function: cacheSolve
## Description:  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Parameter: x

cacheSolve <- function(x, ...) {
        ## Checking if the Inverse is calculated in cache
        inv <- x$get_inv()
        
        ## if is calculated, then get the inverse from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## ...else, Calculate the Inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        ## Set and return the Inverse
        x$set_inv(inv)
        inv
        
}