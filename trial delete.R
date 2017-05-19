## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinv <- function(apple) m <<- apple
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(x$get())
        x$setinv(m)
        m
}


##Example usage

example <- matrix(c(2,2,3,2), nrow=2, ncol=2)
example
a <- makeCacheMatrix(example)
cacheSolve(a)
cacheSolve(a)   
        ## when cacheSolve(a) is run a second time, 
        ## a message appears "getting cached data"
        ## before showing the inverted matrix


rm(list=ls())