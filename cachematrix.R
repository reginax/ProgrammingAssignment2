## This assignment focuses on lexical scoping and caching functions that may require long computation times.
## Below is a pair of functions that cache the inverse of a matrix. 
## makeCacheMatrix creates and returns a special "matrix" or list that can cache its inverse
## cacheinverse calculates the inverse of the matrix using functions stored in the special "matrix" 



# Create a special "matrix" object containing functions: setmatrix, getmatrix, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
    # Args:
    #   x: The inputted matrix 
    #
    # Returns:
    #   A list of functions to set/get the value of the matrix 
    #   and set/get the value of the inverse, using the function solve()
    m <- NULL # sets default value for m
    setmatrix <- function(y) { # function to set the value of the matrix 
        x <<- y 
        m <<- NULL
    }
    getmatrix <- function() x # function to return value of the matrix
    setinv <- function(solve) m <<- solve 
    getinv <- function() m
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setinv = setinv, 
         getinv = getinv)
}


# Compute the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        #
        # Args:
        #   x: The special matrix object 
        #   ...: optional arguments to cacheSolve
        #
        # Returns:
        #   m: The inverse calculated from the data 
        y <- matrix(apply(x$getmatrix(), c(1, 2), solve)) # calculate inverse of the input matrix
        m <- x$getinv() # set m to cached inverse retrieved by getinv function
        if(!is.null(m) && identical(m, y)) { # check if the inverse has already been calculated 
            message("getting cached data")
            return(m) # return cached inverse
        }
        data <- x$getmatrix() # set data to value of the input matrix retrieved by the getmatrix function
        m <- solve(data, ...) # compute value of the inverse of the input matrix x
        x$setinv(m) # cache the inverse with the setinv function 
        m
}