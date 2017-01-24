## To demonstrate Lexical Scoping in R, these functions cache
## inversion of a user-defined matrix so that an expensive operation does not
## need to be performed repeatedly.

## makeCacheMatrix takes an input of a matrix
## It has getters and setters for updating or setting matrix data
## It also initialises the inverse setters and getters so they can be used
## in the other function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        # Assign the input argument to the x object in the parent environment
        x <<- y
        # Assign the value of NULL to the i object in the parent environment
        i <<- NULL
    }
    get <- function() x # Retrieves x from the parent environment
    setinvm <- function(invm) i <<- invm #  i in the parent environment
    getinvm <- function() i # Retrieves i from the parent environment
    
    # Return list of functions to parent environment
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}


## cacheSolve requires an input of type makeCacheMatrix()
## If the makeCacheMatrix object has already been stored it uses
## the cached version of it, otherwise it calculates the inverse matrix

cacheSolve <- function(x, ...) {
    # Requires an input argument of type makeCacheMatrix()
    i <- x$getinvm()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinvm(i)
    return(i)
}

# # Tests
# set.seed(9801)
# x = matrix(rexp(35*35), 35, 35)
# mat = makeCacheMatrix(x)
# 
# # First run, not cached
# head(cacheSolve(mat), 2)
# 
# # Second run cached
# head(cacheSolve(mat), 2)
