## These functions create a computationally CHEAPER method of finding 
## an inverse of a matrix by first setting the values of the matrix and
## caching the value and finally retrieving that value or calculating it

## This function creates a special object that stores a matrix and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
set <- function(y) {
        x <<- y
        inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## this function returns the cached inverse of the first function if it already exists
## If not, it calculates the inverse of 'x'

cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
        return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinverse(inv)
return(inv)
}
