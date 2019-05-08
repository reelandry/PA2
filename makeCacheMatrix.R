makeCacheMatrix <- function(M = numeric(), ...) {
    u <- NULL
    set <- function(y) {
        M <<- y
        u <<- NULL
    }
    get <- function() M
    setinv <- function(solve) u <<- solve
    getinv <- function() u
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}