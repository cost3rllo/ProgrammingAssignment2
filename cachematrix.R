## These function will create and calculate the inverse of a matrix

## Create a matrix object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
        i <<- NULL
        
        set <- function(matrix) {
          x <<- matrix
          i <<- NULL
        }
          
        getm <- function() {
          x
        }
        
        setinv <- function(inverse) {
          i <<- inverse
        }
        
        returninv <- function() {
          i
        }
        
        list(set = set, getm = getm, setinv = setinv, returninv = returninv)

}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## . If the inverse has already been calculated (and the matrix has not
## changed), then the "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$returninv()
        
        if (!is.null(i)) {
            message("getting cache data")
            return(i)
        }
        data <- x$getm()
        i <- solve(data) %*% data
        x$setinv(i)
        i
}
