## makeCacheMatrix creates a special "matrix" containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) i <<- matrix
        getmatrix <- function() i
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

##cacheSolve solves (calculates the inverse of) the matrix created with the above function. 
##However, it first checks to see if the matrix has already been solved. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it solves the matrix and caches its inverse via the 'setmatrix' function.

cacheSolve <- function(x, ...) {
        i <- x$getmatrix()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setmatrix(i)
        i
}
