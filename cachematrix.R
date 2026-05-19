## cachematrix.R
## These two functions cache the inverse of a matrix so that
## the inverse is computed only once and reused on subsequent calls.

## makeCacheMatrix: Creates a special "matrix" object that can
## cache its inverse. It returns a list of 4 functions:
##   set      - store a new matrix (and clear any cached inverse)
##   get      - retrieve the stored matrix
##   setinverse - store a computed inverse in the cache
##   getinverse - retrieve the cached inverse (NULL if not yet cached)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                        # cache starts empty

        set <- function(y) {
                x   <<- y                  # update the stored matrix
                inv <<- NULL               # invalidate the old cache
        }

        get <- function() x                # return the stored matrix

        setinverse <- function(inverse) inv <<- inverse   # write to cache

        getinverse <- function() inv       # read from cache

        list(set        = set,
             get        = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: Returns the inverse of the special "matrix" created
## by makeCacheMatrix. On the first call it computes the inverse,
## stores it in the cache, and returns it. On every subsequent call
## with the same object it retrieves the cached value without
## recomputing.
##
## The matrix x is assumed to be square and invertible.
## Any extra arguments (...) are forwarded to solve().

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()

        if (!is.null(inv)) {               # cache hit → skip computation
                message("getting cached data")
                return(inv)
        }

        data <- x$get()                    # cache miss → compute
        inv  <- solve(data, ...)
        x$setinverse(inv)                  # store result for next time
        inv
}
