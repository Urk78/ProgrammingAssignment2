
## This function returns a list of 4 functions:
## 1) set sets the variable x to y and the variable i to NULL
## 2) get returns the matrix x
## 3) setinverse sets the variable i to inverse
## 4) getinverse returns i

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## This function returns the inverse i of x.
## 1) If the inverse has been calculated before it returns a message and
## the cached inverse
## 2) If the inverse has not been calculated before it gets the matrix, 
## calculates the inverse, caches the inverse in case one might want to use
## it again and finally returns the inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
