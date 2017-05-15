## This function creates a vector that stores 
## 1-sets the matrix, 2-gets the matrix
## 3-sets the inverse of the matrics
## 4-gets the inverse of the matrix

## This vector becomes the input to the cacheSolve function below

makeCacheVector <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This functions calculates the inverse of the vector from above.
## It checks if the inverse has been found. If so, it gets the inverse
## from the cache. Otherwise, it calculates the inverse and sets the results 
## in the cache with the setinv function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
