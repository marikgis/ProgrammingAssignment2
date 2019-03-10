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

# test matrix
#a<-matrix(c(3,8,4,6),nrow = 2,ncol=2,byrow = TRUE)
#a
#      [,1] [,2]
# [1,]    3    8
# [2,]    4    6

# determinant is not 0, thus inverse exists
#det(a)
# [1] -14
# test
#b<-makeCacheMatrix(a)
# grab chached value
#cacheSolve(b)
# [,1]       [,2]
# [1,] -0.4285714  0.5714286
# [2,]  0.2857143 -0.2142857
