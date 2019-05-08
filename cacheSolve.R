cacheSolve <- function(M, ...) {
    u <- M$getinv()
    if(!is.null(u)) {
        message("getting cached data")
        return(u)
    }
    data <- M$get()
    u <- solve(data, ...)
    M$setinv(u)
    u
}