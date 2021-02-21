
## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix x and creates a list of functions.
## set sets the value, get returns the matrix itself
## mat solve is a function that inverts the matrix
##get solve returns the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
 	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        matsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             matsolve = matsolve,
             getsolve = getsolve)

}


## cacheSolve first looks to see if the matrix inversion has been done before.
## If it does then the inversion is returned
## If not, this solves the matrix inversion and records its value in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  	  m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
	  message("solving inverse")
        m <- solve(data, ...)
        x$matsolve(m)
        m
}
}