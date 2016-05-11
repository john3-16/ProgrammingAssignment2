## Matrix inverion is usually a costly computation & there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatly.

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list( set = set, get = get, 
            setInverse = setInverse, getInverse = getInverse )
}#makeCacheMatrix()

## Computes the inverse of the special "matrix" returned by makeCacheMatrix()
## above. If the inverse has already been calculated (and matrix has not changed),
## then the cacheSolve() should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if( !is.null(inv) ){
        message("Result from the cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}#cacheSolve()
