
## creates a special "matrix" object that caches inverse.
makeCacheMatrix <- function( m = matrix() ) {
	## Initialize the inverse property
    i <- NULL
    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    ## Method to get the matrix
    get <- function() {
    	m
    }
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    ## Method to get the inverse of the matrix
    getInverse <- function() {
        i
    }
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## retrieves inverse from the cache
cacheSolve <- function(x, ...) {

    ## Returns a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Returns the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    ## Gets matrix from object
    data <- x$get()
    ## Calculates the inverse using matrix multiplication
    m <- solve(data) %*% data
    ## Sets the inverse to the object
    x$setInverse(m)
    ## Returns the matrix
    m
}
