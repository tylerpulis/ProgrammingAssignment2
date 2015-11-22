## This file contains two functions. The first function will cache
## the inverse of a matrix so it doesn't have to be recomputed. 

## The makeCacheMatrix function creates a special matrix object that 
##contains a function to set and get the value of a matrix and its 
##inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <-function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
        setmatrix = setmatrix, getmatrix = getmatrix)

}


## The second function calculates the inverse of the list created in 
##the first function after first checking to see if the inverse has 
##already been calculated. If the inverse of the matrix is in the 
##cache, it skips the computation and returns the inverse.  
##Otherwise, it calculates the inverse and sets the values of the 
##inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
	 m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m

        ## Return a matrix that is the inverse of 'x'
}
