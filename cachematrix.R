##The following are a pair of functions that cache the inverse of a matrix
## in order to save valuable time wasted in recomputations

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## The set function assigns the value of x and NULL to m
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## The get function gets the value of x
        get <- function() x
        ## The setinverse function assigns the function solve to m
        setinverse <- function(solve) m <<- solve
        ## The getinverse function returns the value of m
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(p, ...) {
        ## Return a matrix that is the inverse of 'p'
        m <- p$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- p$get()
        m <- solve(data, ...)
        p$setinverse(m)
        m
}