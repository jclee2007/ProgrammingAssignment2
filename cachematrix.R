##  The following functions calculates the inverse of a square matrix, assuming 
##  that the matrix is invertible.  To save costly computations, the function will 
##  check if the inverse matrix is already calculated and return in from the 
##  cache.  Otherwise, the function will calculate the inverse matrix and cache it.
##
##
##
##  The makeCacheMatix function creates a special object, which is really a list
##  containing funtions to:
##  1.  Set the square matrix input
##  2.  Get the square matrix
##  3.  Set the inverse matrix
##  4.  Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inverse) {
        inv <<- inverse
    }
    getinv <- function() {
        inv
    }
    list(set = set, get = get,
        setinverse = setinv,
        getinverse = getinv)

}

##  cacheSolve calculates the inverse matrix of the input matrix created with the
##  above function. However, it first checks to see if the inverse matrix  has 
##  already been calculated. If so, it gets the inverse matrix from the cache and
##  skips the computation. Otherwise, it calculates the inverse matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  
  
}
