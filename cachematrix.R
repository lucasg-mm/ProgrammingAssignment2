## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # it will hold the inverse of the matrix x
        inv <- NULL
        
        # sets the matrix x
        set_x <- function(new_x){
                x <<- new_x
                inv <<- NULL
        }        
        
        # gets the matrix x
        get_x <- function() x
        
        # sets the inverse of x
        set_inv <- function(new_inv){
                inv <<- new_inv
        }
        
        # gets the inverse of x
        get_inv <- function() inv
        
        # returns a list with every function
        list(set_x = set_x, get_x = get_x, set_inv = set_inv, get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        # get the value of the inverse of x, and checks if it's not NULL
        inv <- x$get_inv()
        if(!is.null(inv)){
                # if it's not, than just return the inverse
                message("Getting cached data...")
                return(inv)
        }
        
        # -- if it's NULL, calculates the inverse of x
        # gets the matrix
        mat <- x$get_x()
        
        # computes the inverse
        inv <- solve(mat)
        x$set_inv(inv)
        
        # returns a matrix that is the inverse of 'x'
        inv
}
