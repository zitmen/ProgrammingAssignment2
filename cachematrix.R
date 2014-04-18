## Caching the inverse of a matrix
##
## These are actually almost the same as in the example with vector


## Creates a list containing the getter/setter functions to acces the variables
## `x` (the matrix) and `inv` (inverse of the matrix). In fact, these variables
## are not stored inside the list but somewhere in the memory and the functions
## of the list directly access to the address which the variables occupy.
##
## If the data `x` is changed, the inverse is NULLed (the cache gets cleared).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Calculate the inverse of `x`. If the inverse is NULL (the cache has been cleared)
## the inverse is calculated using the `solve()` function. Otherwise the cached value
## is used to save the computational time.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
