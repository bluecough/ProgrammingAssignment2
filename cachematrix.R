## If the contents of a matrix are not changing, 
## it may make sense to cache the value of the matrix so that 
## when we need it again, it can be looked up in the cache rather 
## than recomputed. These below function implements this.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<-y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
    
}


## The following function calculates the inverse Matrix of the special 
## "matrix" created with the above function. 
## However, it first checks to see if the inverse matrix has already 
## been calculated. If so, it gets the inverse matrix from the 
## cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets 
## the value of the inverse matrix in the cache via the setinverse 
## function.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    ## Check to see if there is already an inverse matrix 
    ## if so use the cached version
    if(!is.null(inverse)) {
        message("getting cached data.")
        return(inverse)
    }
    data <- x$get()
    ## solve() does the inverse matrix calculation
    inverse <- solve(data)
    x$setinverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}

