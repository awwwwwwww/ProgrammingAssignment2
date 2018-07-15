## Function - makeCacheMatrix
## Builds a cachable matrix object
## Param x - matrix object
## Return - list containing functions and variables for the cachable matrix

makeCacheMatrix <- function(x = matrix()) {
    # initialize matrix inverse variable
    minv <- NULL
    # function to set matrix and inverse initially
    set <- function(y) {
        # sets matrix at higher environment
        x <<- y
        #sets inverse at higher environment
        minv <<- NULL
    }
    
    # defines function for returning matrix
    get <- function() x
    # defines function for setting inverse
    setinv <- function(s) minv <<- s
    # defines function for returning inverse
    getinv <- function() minv
    #returns list object with functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function - cache solve
## Returns cachable matrix inverse and calculates inverse if necessary
## Param x - matrix object
## Return - Cachable matrix inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()
    # checks if inverse is already computed
    if(!is.null(minv)) {
        # if inverse is computed print that we're using the cached value
        message("getting cached data")
        # return inverse as previously computed
        return(minv)
    }
    # if inverse not previously calculated, get the matrix to be inverted
    data <- x$get()
    # calculate the inverse
    minv <- solve(data, ...)
    # store the inverse to the cachable matrix object
    x$setinv(minv)
    # return the inverse
    minv
}
