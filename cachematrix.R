## This pair of functions cache the inverse of a matrix, which is more efficent
## than calculating it repeatedly. The function first checks to see if the 
## matrix inverse has already been calculated. If so, it returns the value from
## the cache. Otherwise, it will calculate the matrix inverse for the first 
## time and set the value in the cache. 


## Function 1: The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Function 2: The cacheSolve computes the inverse of the special "matrix" by the 
## makeCacheMatrix function above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.  

cacheSolve <- function(x, ...) {
  m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}     
