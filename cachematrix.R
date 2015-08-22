## makeCacheMatrix: function which creates an object to house both matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL   ## inverse of the matrix
    
    ## set: store matrix y, set inverse to uncalculated/NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## get: return the matrix 
    get <- function() x
    
    ## setinv: set the inverse of the matrix
    setinv <- function(inv) i <<- inv
    
    ## getinv: return the inverse of the matrix
    getinv <- function() i
    
    ## show available parts of function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


## cacheSolve: return cached inverse or use solve to calculate inverse
cacheSolve <- function(x, ...) {
    ## read cached inverse
    i <- x$getinv()
    
    ## inverse has not been calculated
    ## calculate with solve(matrix), cache value
    if(is.null(i)) {
        message("calculating inverse")
        i <- solve(x$get())
        x$setinv(i)
    }
    
    ## return inverse
    i
}
