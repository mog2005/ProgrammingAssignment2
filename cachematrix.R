## Put comments here that give an overall description of what your
## functions do

## The functions compute and cache the inverse of an invertible matrix.
## The first time, the functions compute the inverse and save the results
## to the cache.
## Thereafter, the functions skip the computation and get the value of
## the inverse from the cache if the matrix has not changed.

## Write a short comment describing this function

## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## The following function calculates the inverse of the matrix.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse of 
## the matrix in the cache via the setinv function.

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
        ## Return a matrix that is the inverse of 'x'
}
