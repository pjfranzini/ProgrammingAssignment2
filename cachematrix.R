## makeCacheMatrix and cacheSolve work together to save computational time when inverting large matrices
## by caching the inverse

# makeCacheMatrix creates a matrix object which may include info about an inverse
# It has setter/getter methods for the value of the matrix and of the inverse
# Make a cached matrix by running m <- makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
        # initialize a variable to store the inverse
        inv <- NULL

        # Function to set the matrix
        # eg. use by typing m$set( matrix(c(1,2, 11,12), nrow = 2, ncol = 2))
        # Note that when the user changes the matrix by calling m$set, the inverse is set back to NULL
        # thus allowing cacheSolve to know not to return its value, and to recalculate the inverse
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }

        # Function to display the matrix; use by typing m$get()
        get <- function() x

        # setinv and getinv to set/get the inverse; these are used by cacheSolve
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


# invert a cached-matrix-object by running cacheSolve(m); will return the cached inverse and a message
# if the inverse is already calculated; otherwise calculates the inverse
cacheSolve <- function(x, ...) {
     # get the cached inverse property of the matrix object
     inv <- x$getinv()
     # if it isn’t null we have a useful cache of the inverse, output it
     if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
     }
     # otherwise pull the actual matrix data and invert it, and set the inverse property, and display the result
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
 }