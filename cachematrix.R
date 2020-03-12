## Put comments here that give an overall description of what your
## functions do
## The pair of functions, namely, "makeCacheMtrix" and "cacheSolve" can cache 
## the inverse of a matirx

## Write a short comment describing this function
## makeCacheMatrix is a function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i_m <- NULL
    set <- function(y) {
        x <<- y
        i_m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i_m <<- solve
    getinverse <- function() i_m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve is a function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i_m <- x$getinverse()
    if(!is.null(i_m)) {
        message("getting cached data")
        return(i_m)
    }
    mat <- x$get()
    i_m <- solve(mat, ...)
    x$setinverse(i_m)
    i_m
}