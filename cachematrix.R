## Matrix inversion is computed and cached using this two functions
## This function takes ONLY invertible matrices 
##                              EXAMPLE: 
## trial_a <- matrix(1:4, 2, 2)
## cache_a <- makeCacheMatrix(trial_a)
## cacheSolve(cache_a) ## the computed inverted matrix should be cached

##makeCacheMatrix() produce results of "set", "get", "setinverse" & "getinverse"
## This essentially caches the matrix for cacheSolve() to work on
## makeCacheMatrix(trial_a)$get() produces the original matrix input
## makeCacheMatric(trial_a)$set() allows for a new matrix input to overwrite 
## original

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() solves the cached matrix
## The solved matrix is then cached in makeCachedMatrix(trial_a)$getinverse
## Each subsequent time the solved matrix is called, the solved matrix will be 
## called from the cached data instead of solving for the matrix again.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
