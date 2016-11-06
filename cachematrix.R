## This functions create a special matrix object in the cache and computes 
## the inverse this matrix and caches 

## Create a matrix object in the cache

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) invert <<- inverse
        getinv <- function() invert
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Compute the inverse of the matrix returned by makeCacheMatrix. If the matrix
## change, then calculated inverse in the cache, else return actual inverse in cache.

cacheSolve <- function(x, ...) {
        invert <- x$getinv()
        if(!is.null(invert)) {
                message("getting cached data")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setinv(invert)
        
        return(invert)
        ## Return a matrix that is the inverse of 'x'
}
