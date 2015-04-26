## R Programming Assignment 2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly in addition to other 
## alternatives. These functions set/get an inverted matrix 
## to and from cache. Assumes input is an invertible matrix.
     

## makeCacheMatrix: This function creates a special "matrix" 
## object and functions.
makeCacheMatrix <- function(x = matrix()) {
        m <- matrix(data = NA)

        ## define the needed matrix functions
        ## apply to global environment (<<-)
        set <- function(y) {
                x <<- y
                m <<- matrix(data = NA)
        }
        get <- function() x
        setInverse <- function(input) m <<- input
        getInverse <- function() m

        ## store these functions in a list
        ## being in a list allows use of $function
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) 
}


## cacheSolve: This function computes the inverse of a "matrix". 
## and assumes that the matrix is invertible. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.na(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()

        ## Compute the inverse of matrix when inverse not cached
        ## Can use ginv() [MASS pkg] or solve() - using solve here     
        m <- solve(data)
        x$setInverse(m)
        m
}
