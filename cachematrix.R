## Put comments here that give an overall description of what your
## functions do


## The "makeCacheMatrix" function creates a special "matrix" object 
## that can cache its inverse.
# It contains the following functions:
# - Set()      set the value of a matrix
# - Get()      get the value of a matrix
# - SetInverse()   get the cached value (inverse of the matrix)
# - GetInverse()     get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        # store a matrix
        set <- function(y) {
                x <<- y
                ## since matrix assigned new value, flush the cache
                m <<- NULL
        }
        
        # return the stored matrix
        get <- function() x
        
        ##cache the given argument
        setinverse <- function(solve) m <<- inverse
        
        ##Get the cache Value
        getinverse <- function() m
        
        ##return a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The "cacheSolve" function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ##if Cache exists return it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##Otherwise get the matrix, inverse it and return and set Cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
