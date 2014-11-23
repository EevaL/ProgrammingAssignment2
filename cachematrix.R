## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a version of the given matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		## list of different "functions"
        m <- NULL
        set <- function(y) {
				## sets the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x
		## returns the matrix
        setsolve <- function(solve) m <<- solve
		## saves the solved matrix
        getsolve <- function() m
		## returns the solved matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve calculates the inverse of the special matrix created by makeCacheMatrix (if it is not calculated earlier)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  m <- x$getsolve()
        if(!is.null(m)) {
				## if the inverse is cached, this will return the cached version
                message("getting cached data")
                return(m)
        }
        ## if the inverse is not cached, it will be solved (and returned) here
		data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

