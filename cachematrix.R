## Calculates the inverse of a matrix and caches the
## result in a different environment than that of the function.
## The cache result is returned when the same initial
## matrix is used instead of recomputing the inverse.

## Usage: 
## Use makeCacheMatrix as constructor to create
## a function object, for example:
##      cachematrix <- makeCacheMatrix()
## Initialize this with a matrix, m, using the set method
## of the object, for example:
##      cachematrix$set(m)
## Calculate or retrieve the inverse matrix using the matrix object
## as an argument to the cacheSolve function,  
##      inv_matrix<-cacheSolve(cachematrix)

makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix assigns a matrix to an object in a parent
## enviroment to the function where the assignment occurs

    m <- NULL
    set <- function(y) {
        x<<- y
        m <<- NULL
    }
    get <- function() x 
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, 
         getsolve = getsolve)
}




cacheSolve <- function(x, ...) {
## calculates a matrix that is the inverse of an input matrix
## after checking to see if the inverse has previously been
## calculated and cached

    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
