## makeCacheMatrix creates a list of function which do the following
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y){
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinverse <- function(im) invmat <<- im
    getinverse <- function() invmat
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created with the previous function. 
## It first checks if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix sets the value of the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinverse()
    if(!is.null(invmat)){
        message("getting cached matrix")
        return(invmat)
    }
    invmat <- solve(x$get())
    x$setinverse(invmat)
    invmat
}
