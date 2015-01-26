## These two functions will store a matrix and cache
## its inverse, respectively.

## This first function will use several functions to 
## set and get the value of a matrix, and then set and
## get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) im <<- inverse
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks whether the inverse has already
## been cached.  If not, it calculates the inverse of
## the matrix and caches it

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
