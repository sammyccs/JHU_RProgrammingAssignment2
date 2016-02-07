
#Write the following functions for R programming assignment:

#1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#For this assignment, assuminmg that the matrix supplied is always invertible.

##  A pair of functions were programmed to cache and compute the inverse of a matrix.

## 1 makeCacheMatrix: 
makeCacheMatrix <- function(x = matrix()) { ## create a function to set an inverse matrix
    inv_m <- NULL
    set <- function(y) {
        x <<- y; ## if two matrices are identical, skip the following matrix 
        inv_m <<- NULL; ## set it as null
    }
    get <- function() return(x); 
    setinv <- function(inv) inv_m <<- inv; ## set a funtion to inverse the matrix
    getinv <- function() return(inv_m); 
    return(list(set = set, get = get, setinv = setinv, getinv = getinv)) ## list up the values from results
}

## 2 cacheSolve:
cacheSolve <- function(x, ...) { ## create a function to cache the inverse of matrix
    inv_m <- x$getinv()
    if(!is.null(inv_m)) { ## create a set function to see the following test if not null, two matrice are not the same
        message("Getting cached data...") ## obtain the inverse from cache 
        return(inv_m) ## otherwise, retrun the inverse matrix
    }
    data <- x$get() ## set the data
    inv_m <- solve(data, ...) ## create a function solve for its inverse from data 
    x$setinv(inv_m)
    return(inv_m) ## retrun the inverse matrix
}

