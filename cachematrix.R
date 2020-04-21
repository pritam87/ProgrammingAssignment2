
makeCacheMatrix <- function(x = matrix()) {
    ## Creates a special "matrix" object that can cache its inverse.

    inv <- NULL                                         # initialize inverse with NULL
    
    setMatrix <- function(m) {                          # mutator function for setting matrix
        x <<- m
        inv <<- NULL
    }
    
    getMatrix <- function() x                           # accessor function for getting matrix
    
    setInverse <- function(inverse) inv <<- inverse     # mutator function for setting inverse
    
    getInverse <- function() inv                        # accessor function for getting inverse
    
    list(set = setMatrix, get = getMatrix, 
         setinverse = setInverse, 
         getinverse = getInverse)               # return a list containing all accessor & mutator functions
}


cacheSolve <- function(x, ...) {
    ##  Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
    ##  If the inverse has already been calculated (and the matrix has not changed), 
    ##  then it retrieves the inverse from the cache.
    
    inv <- x$getinverse()           # attempts to fetch cached value of inverse
    
    if(!is.null(inv)) {             # checks whether a cached value exists
        return(inv)                 # if it exists, returns cached value of inverse
    }
    
    data <- x$get()                 # otherwise, fetches the new/updated matrix
 
    inv <- solve(data, ...)         # computes the inverse of matrix fetched above 
    
    x$setinverse(inv)               # caches the value of inverse computed above
    
    inv                             # returns the freshly computed value of inverse
}
