rm(list=ls())
makeCacheMatrix <- function(M = matrix()) {
    inv <- NULL
    set <- function(x) {
        M<<- x
        inv <<- NULL
    }
    get <- function() M
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



cacheSolve <- function(M, ...) {
    inv <- M$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- M$get()
    inv <- solve(data)
    M$setinverse(inv)
    inv
}

