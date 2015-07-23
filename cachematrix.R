## Compute the inverse of a matrix then cache the result 

## makeCacheMatrix has 4 functions set, get, setminv, getminv
## set changes the vector stored in the main function
## get returns the vector stored in the main function
## setinv changes the vector stored in the main function
## getinv returns the vector stored in thhe main function

## it is possible to pass a new value into any of the set 
## functions.  The result in get is the new value. The
## inverse is not computed here!
makeCacheMatrix <- function(x = matrix(c(1:4), nrow = 2, ncol = 2)) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setminv <- function(solve) m <<- solve
        getminv <- function() m
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}


## cacheSolve computes the inverse of matrix stored in makeCacheMatrix
## this function only works if run after makeCacheMatrix e.g.
## a<-makeCacheMatrix(b)
## cacheSolve(a)
## this funciton only computes the inverse if the solution is not already
## cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getminv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setminv(m)
        m
}
