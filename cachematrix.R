
## Creates a list of functions for set, get, setInverse and getInverse of a matrix in a local context 
## in order to store both matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #setting the matrix - reset the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inv <<- i
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Does the solve (inverse) of a matrix and caches it or uses the cached result if available

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        #check if we have an inverse in cache
        if(!is.null(inv))
                return(inv)
        #if not, calculate and cache it
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
