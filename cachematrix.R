## Caching the Inverse of a Matrix:
## Below are two functions that are used to create a special object that stores a matrix 
## and caches its inverse.



## The first function makeCacheMatrix creates a list in that we can store the matrix and cache 
##the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
    inversematrix <- NULL
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    get <- function() {
        x
    } 
    setInverse <- function(inverse) {
        inversematrix <<- inverse
    }
    getInverse <- function() {
        inversematrix
    }
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The second function shows the inverse of the matrix which is created by the makeCacheMatrix
## function above. If the inverse has already been calculated, and the  matrix has not changed,
## then the cacheSolve functuon will retrieve the inverse from the cache. If the inverse has not
## been calculated, the cacheSolve functuon will calculte the inverse and returns it as the result.
## In the meantime, the cacheSolve functuon stores the reslut in the objet "setInverse".

cacheSolve <- function(x, ...) {
    inversematrix <- x$getInverse()
    if(!is.null(inversematrix)) {
        message("getting cached data")
        return(inversematrix)
    }
    data <- x$get()
    inversematrix <- solve(data, ...)
    x$setInverse(inversematrix)
    inversematrix            ## Return a matrix that is the inverse of 'x'
}


