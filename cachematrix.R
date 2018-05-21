## R function to get, set, getInverse and setInverse of an invertible matrix
## x is an object of Matrix type

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverseMatrix <<- solve
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## R function to check if inverse of matrix already exists in memory
## If inverse already exists, return the same. Else, calculate the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    ## Check if the inverse of matrix exists in cache
    if(!is.null(inverseMatrix)) {
        message("getting cached matrix")
        return(inverseMatrix)
    }
    myMatrix <- x$get()
    inverseMatrix <- solve(myMatrix, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
