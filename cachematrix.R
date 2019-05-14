## The functions below compute the inverse of a matrix and cache the result.
## If the inverse is asked, the functions return the cache result instead
## of computing again the result.

## This function creates an object that can store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv_mat <<- solve
        getinv <- function() inv_mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function sets the new object and returns the inverse of the matrix,
## either by computing the inverse or retrieving the resultfrom cache if it 
## was already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getinv()
        if(!is.null(inv_mat)) {
                message("getting cache data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$setinv(inv_mat)
        inv_mat
}
