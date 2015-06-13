## These two fuctions cache the inverse of a matrix. If the inverse has not
## been computed before, then compute the inverse and cache it. If the inverse
## already exists, then return it.

## This function is used to create a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(mx = matrix()) {
        inv <- NULL
        set <- function(a) {    
                # change the stored matrix and reset cached inverse
                mx <<- a
                inv <<- NULL
        }
        get <- function() mx    # return the stored matrix
        set_inverse <- function(b) inv <<- b    # change the cached inverse
        get_inverse <- function() inv    # return the chched inverse
        list(set = set, get = get,    #return a list of 4 functions
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## This function computes the inverse of a special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        # take special "matrix" object as parameter
        inv <- x$get_inverse()    # read the cached inverse
        if(!is.null(inv)) {
                # if cached inverse exists, then return it without calculating
                message("getting cached data")
                return(inv)
        }
        # if cached inverse doesn't exist, then
        # do below to compute and cache it
        data <- x$get()    # read the stored matrix
        inv <- solve(data, ...)    # compute the inverse
        x$set_inverse(inv)    # cache the inverse
        inv
}
