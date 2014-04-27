##  Matrix inversion is usually a costly computation and their may be some benefit 
##  to caching the inverse of a matrix rather than compute it repeatedly 
##  (there are also alternatives to matrix inversion that we will not discuss here). 
##  Here is a pair of functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.
##  For this assignment, we assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        
        # init inverse of matrix to NULL
        inv_m <- NULL
        # set value of matrix
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }        
        get <- function() x
        setInverse <- function(solve) inv_m <<- solve
        # set value of inverse of matrix
        getInverse <- function() inv_m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##  above. If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getInverse()
        # use cached inverse matrix value if already cached
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        # else compute inverse of matrix
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setInverse(inv_m)
        inv_m
}
