## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        ## Set function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Get function
        get <- function() x
        
        ## Set inverse
        setinverse  <- function(inverse) m  <<- inverse
        
        ## Get inverse
        getinv <- function() m
        
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
             

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- getinverse()
        
        ## Just return calculated inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Calculate inverse
        data <- x$get()
        m <- solve(data, ...)
        
        ## Cache the inverse
        x$setinverse(m)
        
        ## Return
        m
        

}
