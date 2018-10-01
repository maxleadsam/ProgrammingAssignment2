

## Programming Assignment 2. Created by Sam Vermeerssen

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL             ## initiates the inverse
        set <- function(y) {  ## set function is defined
                m <<- y       ## source matrix to parent frame
                i <<- NULL    ## used to reset i for the new m
                              ## inverse to parent frame 
        }
        get <- function() m   ## get function is defined
        setinverse <- function(solve) i <<- solve  ## setinverse function is defined
        getinverse <- function() i      ## getinverse function is defined
        list(set = set, get = get,      ## a list is returned with the names of the functions 
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(m, ...) {    ## inverse of special matrix inversed
        i <- m$getinverse()         ## attempt to retrieve cached inverse
        if(!is.null(i)) {           ## cached inverse is returned if the inverse is present 
                message("getting cached data")
                return(i)
        }
        data <- m$get()             ## calculate the inverse of the matrix again
        i <- solve(data, ...)       ## if inverse does not exist
        m$setinverse(i)             ## set the cached inverse through setter
        i                           ## inverse matrix is returned 
}
