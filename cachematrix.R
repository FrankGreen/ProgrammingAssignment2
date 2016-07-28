## The makeCacheMatrix function allows the cacheSolve function to store the
## inverse of the matrix x.  The benefit of this is that if the inverse of 
## the matrix has already been solved in cacheSolve, the solution can be pulled
## from the cache instead of having to recalculate it.  If this were part of a
## loop this could save processing time as the inverse would only have to be
## solved once during the loop provided the matrix x did not change.


## makeCacheMatrix sets up variables that will store the matrix X and the inverse
## in the parent environment.  This way the matrix and the inverse are accessible
## by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve will check to see if the inverse for the matrix x has already
## been calculated.  If it has, then it will pull the inverse from the cached
## data.  If not, it will solve for the inverse and store that value first as i
## in the cacheSolve environment and then in the parent environment using
## setinverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


## Test of functions to solve inverse for provided matrix x:

## > x = matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
