## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(M = matrix()) {
    W <- NULL
    set <- function(y) {
        M <<- y
        W <<- NULL
    }
    get <- function() return(M)
    setinv <- function(inv) W <<- inv
    getinv <- function() return(W)
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(M, ...) {
    W <- M$getinv()
    if(!is.null(W)) {
        message("getting cached data")
        return(W)
    }
    data <- M$get()
    W <- solve(data, ...)
    M$setinv(W)
    return(W)
}
