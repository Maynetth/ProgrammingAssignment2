## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This has founctions that cache the inverse of a function
## The function makes a unique "matrix" object that caches the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function solves for the inverser of the unique "matrix" returned by the makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Solving cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
