
################################################################################
## File: cachematrix.R
## Author: César Pinto. Chile
## Original Source: Forked from https://github.com/rdpeng/ProgrammingAssignment2
################################################################################
## DESCRIPTION
##
##The following functions (makeCacheMatrix & cacheSolve) cache the inverse of a 
##matrix rather than compute it repeatedly 
#################################################################################





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
        ##Parameter: y matrix()
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Checking if the Inverse is calculated in cashe
        inv <- x$get_inv()
        
        ## if is calculated, then get the inverse from cashe
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