## The first function creates a matrix object that has the ability to cache it's inverse
## The second function computes the inverse of the matrix.  If the inverse has already been computed for that particular
## matrix then cachesolve will retrive the inverse from the cache

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
