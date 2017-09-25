## Since computing the inverse of a matrix can be a time consuming job, these two functions will try to limit spent time by 
## first checking whether the inverse is already cached. When this is the case, the cacheSolve function will not start to compute again,
## but will just show the already cached inverse. If the inverse is not cached yet, cacheSolve will compute the inverse and cache the 
## result.

## makeCacheMatrix will make a list of four functions. These four functions will be used further in the cacheSolve>

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## If the inverse of the matrix is already stored in makeCacheMatrix, the first part of this function will "get" the inverse and 
## print it. Otherwise the function will "get" the matrix, inverse it, and then "set" or cache the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
