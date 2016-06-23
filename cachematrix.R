## Put comments here that give an overall description of what your
## functions do
## Here I am trying to create a function which can cache 
## potentially time-consuming computations
## What I have observed from instructions and comments that
## Matrix inversion is usually a costly computation
## and their may be some
## benefit to caching the inverse of a matrix rather than compute it

## Write a short comment describing this function
## This function, `makeCacheMatrix` creates a special "matrix" object,
## which can cache its inverse

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


## Write a short comment describing this function
## This function computes the inverse of special "matrix" created in
## makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mt <- x$get()
        i <- solve(mt, ...)
        x$setInverse(i)
        i
}
