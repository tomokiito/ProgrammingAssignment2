## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        
        # set the value of the Inverse of a Matrix
        setsolve <- function(solve) s <<- solve
        
        # get the value of the inverse of a Matrix
        getsolve <- function() s
        
        #answer
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        
        # if the inverse has already been calculated,
        # it gets the inverse from the cache and skips the computation
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        # else, calculates the inverse of the data
        data <- x$get()
        s <- solve(data, ...)
        # sets the value of the inverse in the cache
        x$setsolve(s)
        
        #answer
        s
}
