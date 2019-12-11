## Put comments here that give an overall description of what your
## functions do

# First, we make a function that creates a matrix object that can cache its
# inverse, completely parallel to the one  in the example. 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# This function computes the inverse
# If the inverse has already been calculated, `cacheSolve` gets
# the inverse from the cache, otherwise it newly calculates it. 
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

# Small example: 

a <- matrix(c(1,2,3,4), nrow=2, ncol = 2)
b <- makeCacheMatrix(a)

cacheSolve(b)
