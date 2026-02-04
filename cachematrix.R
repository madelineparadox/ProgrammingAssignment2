## Put comments here that give an overall description of what your
## functions do

## Create a matrix that can catche its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ##value not yet calculated
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes inverse of makeCacheMatrix. 
## If the inverse has been calculated and matrix
## hasn't been changed, should retrieve inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
