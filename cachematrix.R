##The functions below provide an easy method of storing the inverse of an unchanging
##matrix so that it does not need to be re-computed each time it is required


## This function creates a "special" matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function is used to check whether the inverse of given matrix has already
##been cached, and if not, to then compute it using solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("fetching cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) ##this is where the magic happens! 
    x$setinverse(inv)
    inv
}
