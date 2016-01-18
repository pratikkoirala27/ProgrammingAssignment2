## The following functions caches the inverse of a function.
## The two functions below creates an object that stores the matrix and caches
## its inverse

## The first function, makeCacheMatrix creates a special "matrix" which can cache
## its own matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the matrix created by the 
## function above. It checks to see if the inverse has already been calculated.
## If so, the function gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setInverse(i)
        i
}
