## makeCacheMatrix takes a matrix as its input and creates a special "matrix" 
## object.cacheSolve takes this "matrix", calculates its inverse, stores it in 
## the cache and returns it. If the inverse is already calculated, cacheSolve 
## will  retrieve it from the cache and skip the computation. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This is done via a list of functions that set and get the value both of the 
## matrix and of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve calculates the inverse of the "matrix" created with the 
## makeCacheMatrix and sets its value in the cache via the setsolve function. 
## If the inverse is already calculated, it is retrieved from the cache
## via the getsolve function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
