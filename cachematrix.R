## makeCacheMatrix is a function that creates a matrix that can cache
## its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
               
                 x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## This functions returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        inv <- x$get_inv()
        if(!is.null(inv)) 
                {
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        
        x$set_inv(inv)
        inv
}

