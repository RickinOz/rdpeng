## This function cache the inverse of a matrix and store the matrix for future reference
## It has 2 sub-functions: 1-the first one to cache the inverse of a matix
##                         2-the second check the cache before inverting a matrix

##Inserve a matrix and store result for future reference
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##Test to see if matix exist else inverse matrix with formula in first function
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
