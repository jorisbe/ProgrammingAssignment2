## Functions to cache the inverse of a matrix to reduce computing needs
## 

## Matrix with function to get and set the matrix, and the inverse

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(invMatrix) invM <<- invMatrix
    getinverse <- function() invM
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## returns the inverse of a matrix, uses a cached version if possible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invM <- x$getinverse()
    if(!is.null(invM)){
        return(invM)
    }
    matrix <- x$get()
    invM <- solve(matrix)
    x$setinverse(invM)
    invM
    
}
