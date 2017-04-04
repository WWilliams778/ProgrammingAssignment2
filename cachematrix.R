## The following function creates a matrix object that can cache its inverse,
## containing functions set, get, setinverse, and getinverse.

## set - sets/changes the value of the matrix in the main function
## get - returns the value of the matrix
## setinverse - stores the value of the inverse 
## getinverse - returns the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function calculates the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (i.e.
## the matrix has not changed), the cacheSolve should retrieve the inverse from the cache.
## If not, data gets the matrix cached with the above function (makeCacheMatrix), m solves
## for the inverse, x$setinverse stores result as m in above function (makeCacheMatrix)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
