
#Write the following functions for R programming assignment:

#1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#For this assignment, assuminmg that the matrix supplied is always invertible.

##  A pair of functions were programmed to cache and compute the inverse of a matrix.

## 1 makeCacheMatrix: 
makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
        x <<- y;
        inv_m <<- NULL;
    }
    get <- function() return(x);
    setinv <- function(inv) inv_m <<- inv;
    getinv <- function() return(inv_m);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## 2 cacheSolve:
cacheSolve <- function(x, ...) {
    inv_m <- x$getinv()
    if(!is.null(inv_m)) {
        message("Getting cached data...")
        return(inv_m)
    }
    data <- x$get()
    inv_m <- solve(data, ...)
    x$setinv(inv_m)
    return(inv_m)
}

