#  The following code consists of two function:  "makeCacheMatrix"; and "cacheSolve".
#
#  makeCacheMatrix is intended to create a "special matrix object" that can cache its inverse.
#  The object created by makeCacheMatrix is of class "list".
#
#  cacheSolve is intended to calculate the inverse of the "special matrix object" that is 
#  returned when makeCacheMatrix is called.  However, cacheSolve first checks to see if the inverse 
#  has already been calculated. If the inverse has already been calculated, cacheSolve gets the inverse
#  from the cache and skips the calculation.  Otherwise, cacheSolve calculates the inverse and saves the
#  results of that calculation to the cache.

#  The purpose of the functions is to avoid potentially time-consuming calculations.
#  Calculating the inverse of a large matrix can be time-consuming, especially if it has to be calculated 
#  repeatedly (e.g. in a loop). Accordingly, if a matrix's content remains the same, it may make sense
#  to cache the value of its inverse so that if the inverse is needed again, the inverse can be looked up in
#  the cache rather than recalculated.  



makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() {
                x
        }
        setSolve <- function(solve) {
                s <<- solve
        }
        getSolve <- function() {
                s
        }
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}


