 
#  Here, Two functions are created to cache the inverse of a matrix.

# First function:makeCacheMatrix, that creates a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set<- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Second function: cacheSolve, that returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, It gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
      
      ## Sample run:
## x= matrix(1:4,2,2)
## > m=makeCacheMatrix(x)
## > m$get()
##    [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(m)
##    [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data.
##    [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
