 ## Below are two functions that are used to create a special object that
##  stores a matrix and cache's its inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) ## sets the value of x
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x ## gets the value of x
        
        setInverse <- function(Inverse) m <<- Inverse ## sets the inverse matrix
        
        getInverse <- function() m ## gets the inverse matrix
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        ## produces a special matrix (a list of functions )

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        m <- x$getInverse()
        
        if(!is.null(m))  ##check if the inverse has already been calculated 
        {          
                message("getting cached data")
                
                return(m)## retrieves the inverse from the cache
        }
        
        data <- x$get() ## computes the inverse if it has not been calculated
                        ## and m has not changed
        
        m <- solve(data, ...)
        
        x$setInverse(m)
        
        m ## Returns a matrix that is the inverse of 'x'
}
