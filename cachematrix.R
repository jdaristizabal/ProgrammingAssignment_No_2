## These two functions cache the inverse of a given matrix

## This first function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## Set the matrix
        
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        ## Get the matrix
        
        get <- function() {
                ## Return the matrix
                m
        }
        
        ## Set the inverse
        
        setInverse <- function(inverse) {
                i <<- inverse
        }
        ## Return the inverse
        
        getInverse <- function() {
                i
        }
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This fuction computes the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## JIf it is already set, it just returns the matrix
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set the inverse
        x$setInverse(m)
        
        ## Return matrix
        m
}