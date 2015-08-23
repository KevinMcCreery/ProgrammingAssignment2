## Purpose is to enable caching of matrix invesrse so that solve function doesn't have to 
##be re-run when the matrix hasn't changed and the inverse was already calculated
##This assumes the matrix is non-singluar and is invertible




# makeCacheMatrix creates a matrix that can be cached, 
#so that costly inverse calc result is re-used

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(minverse) inv <<- minverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}



# Present the cached matrix if nothing has changed, 
#or calculate the inverse matrix and present it

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
