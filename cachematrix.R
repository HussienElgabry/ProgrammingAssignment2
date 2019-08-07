## Functions that inverse a matrix. Those functions also cache the results
## so that computations are not needed to be done multiple times
## for the same matrix


## initialise the vector to contain the matrix and its cashed inverse
makeCacheMatrix <- function(x = matrix())
{
    m <- NULL

    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## returns (or computes if not cached) the inverse of the matrix and sets the cache
cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
