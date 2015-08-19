## Functions 'makeCacheMatrix' and 'cacheSolve' work together to compute and cache
## a matrix 'x' and its inverse as computed by solve(). 'cacheSolve' uses the functions
## defined in 'makeCacheMatrix' and examines whether the cache stores the appropriate
## matrix and inverse for an 'x' in use.

## To use these two functions, you need to pass matrix(x) as an argument through
## the constructor function makeCacheMatrix() first and assign that to a new variable
## (let's call it 'z'). It is variable 'z'that we must pass through cacheSolve.

## The function 'makeCacheMatrix' stores a list of four functions: 
## 1. 'set' creates a matrix which is needed when you change the argument 'x' through 
## repeated use 
## 2. 'get' will pass the matrix to cacheSolve to be worked on for.
## 3. 'setinverse' creates the matrix object's inverse using the solve() function, 
## this will be used by cacheSolve if the inverse matrix is not yet cached.
## 4. 'getinverse' will be passed through cacheSolve to reach the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y         ## The matrix is updated in the global environment 
                inv <<- NULL    ## with the input by the user. No inverse set or cached yet.
        }
        get <- function() x   ## Simply returns matrix 'x'.
        setinverse <- function(solve) inv <<- solve ## Inverse of 'x' is calculated.
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The objective function `cacheSolve`takes the matrix created by 'makeCacheMatrix' 
## and according to whether the inverse of the matrix is stored in 
## 'makeCacheMatrix' as 'getinverse', proceeds to 1. either get the data from
## the cache ('getinverse') or 2. to compute it anew by using solve().

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) { ## Tests if the inverse is already cached by makeCacheMatrix
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()         ## Points to data computed by 'makeCacheMatrix'
        inv <- solve(data, ...) ## Proceeds to compute new inverse for the matrix 'x' as        
        x$setinverse(inv)      ## computed by the function 'get' in makeCacheMatrix.
        inv ## Returns a matrix that is the inverse of 'x'.
}