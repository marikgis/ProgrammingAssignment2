## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Accepts matrix as a argument and saves the inverse in a cache (different session)
# This function assumes that inverse for the input matrix exists

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(matinverse) i <<- matinverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

# returns data from the cache (if it exists)

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("retrieving from the cache")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
