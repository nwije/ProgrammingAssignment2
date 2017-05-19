## OVERALL DESCRIPTION
        ## This program is broken into two functions to return the
        ## inverse of an invertible matrix, and return the inverse from the cache
        ## if the inverse has been calculated previously.
        ## The first function allows the matrix to be inverted to be entered, and
        ## clears the cache. The second function checks the cache to see whether
        ## the inverse exists. If it does, the cached inverse is returned.
        ## If it does not exist, then the inverse is computed and stored in the cache
        ## An example is given at the end of this program.





## Write a short comment describing this function: makeCacheMatrix
        ##makeCacheMatrix creates a list of functions that is needed by 
        ##cacheSolve. It stores the matrix to be inverted, and clears the cache
        ##so that previous calculations are not returned by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinv <- function(invert) m <<- invert
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}






## Write a short comment describing this function: cacheSolve
        ## cacheSolve returns the inverse of the matrix entered in the 
        ## previous function (makeCacheMatrix) from the cache if it exists.
        ## If the inverse matrix is not in the cache, it creates the inverse matrix
        ## and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(x$get())
        x$setinv(m)
        m
}

##Example usage

example <- matrix(c(2,2,3,2), nrow=2, ncol=2)
a <- makeCacheMatrix(example)
cacheSolve(a)
cacheSolve(a)   


## when cacheSolve(a) is run a second time, 
## a message appears "getting cached data"
## before showing the inverted matrix



