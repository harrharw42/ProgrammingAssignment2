## Simple set of functions to: 
## 1. create an "extended matrix" that holds functions to get/set the matrix value and the inverse matrix value
## 2. calculate the inverse of a matrix and cache the value

## function:  makeCacheMatrix
## Creates a "special matrix" with functions to get/set the matrix instance as well as to get/set the inverse of the matrix
## Note:  it is assumed that the matrix passed in is an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverse) inverseMatrix <<- newInverse
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## function cacheSolve
## Accepts a matrix created via "makeCacheMatrix" and will do the following:
## return the cached inverse matrix if available
## calc the inverse of the matrix, cache it, and return the value

cacheSolve <- function(x, ...) {
    inverseToReturn <- x$getInverse()
    if(!is.null(inverseToReturn)) {
        message("getting cached data")
        return(inverseToReturn)
    }
    origMatrix <- x$get()
    inverseToReturn <- solve(origMatrix)
    x$setInverse(inverseToReturn)
    inverseToReturn
}
