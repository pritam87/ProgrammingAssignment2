
makeCacheMatrix <- function(x = matrix()) {
    ## Creates a special "matrix" object that can cache its inverse.

    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ##  Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
    ##  If the inverse has already been calculated (and the matrix has not changed), 
    ##  then it retrieves the inverse from the cache.
    
    inv <- x$getinverse()   # attempts to fetch cached value of inverse
    
    if(!is.null(inv)) {     # checks whether a cached value exists
        return(inv)         # if it exists, returns cached value of inverse
    }
    
    data <- x$get()         # otherwise, fetches the new/updated matrix
 
    inv <- solve(data, ...) # computes the inverse of matrix fetched above 
    
    x$setinverse(inv)       # caches the value of inverse computed above
    
    inv                     # returns the freshly computed value of inverse
}
