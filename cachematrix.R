## These two functions create a matrix object and calculates and caches its inverse,
## and returns the inverse but from the cache if the matrix hasn't changed.

## This function creates an object matrix that has functions:-
## get : for returning the matrix it has taken as an argument
## set : for setting new value of the matrix (and wiping the cached matrix value)
## setinverse : for setting the cache value for the matrix inverse (m, in parent environment)
## getinverse : for returning the matrix inverse cache value (m, in parent environment) 

makeCacheMatrix <- function(x = matrix()) {

            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) m <<- solve
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}

## This function calculates the inverse of the matrix stores it in cache
## (m, in parent environment).
## If the inverse has already been calculated and/or set with 'setinverse' and
## the matrix hasn't changed, then this function will retrieve and return the inverse
## from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'

            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data)
            x$setinverse(m)
            m
 
}

