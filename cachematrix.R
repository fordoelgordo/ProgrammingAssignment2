## Finding the inverse of a matrix can be a very time-consuming computation.  Because of this, there may be some 
## benefits to caching the inverse of a matrix rather than allowing it to compute repeatedly.  The functions below
## cache the inverse of a matrix x and return it.

## makeCacheMatrix creates a list containing a function to set the value of the matrix, get the value of the matrix,
## set the value of the inverse of the matrix and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x<<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve returns the inverse of matrix x.  If the invrese has already been computed, then it pulls the value from
## the cache.  If the inverse has not been computed, it sets the value in the cache using the setinverse function
cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
       if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
