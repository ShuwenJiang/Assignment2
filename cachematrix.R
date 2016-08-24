## this file aims to cache the inverse of a matrix, assuming the matrix
##is invertible

##makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse matrix
##4. get the value of the inverse matrix
makeCacheMatrix <- function (x= matrix()) {
    inmatrix <- NULL
    set <- function (y) {
        x <<- y
        inmatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inmatrix <<- solve
    getinverse <- function() inmatrix
    list (set = set, get = get, 
          setinverse = setinverse, getinverse = getinverse)
}

##The following function calculates the inverse of the special "matrix"
## created with the above function.
cacheSolve <- function(x, ...) {
    inmatrix <- x$getinverse()
    if (!is.null(inmatrix)) {
        message("getting the cached data")
        return(inmatrix)
    }
    data <- x$get()
    inmatrix <- solve(data, ...)
    x$setinverse(inmatrix)
    inmatrix
}
