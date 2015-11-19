## There are two functions in this script.
## 1. makeCacheMatrix : This function creates a special "matrix" object that
##                      can cache its inverse
## 2. cacheSolve : This function computes the inverse of the special "matrix"
##                 returned by makeCacheMatrix above. If the inverse has
##                 already been calculated (and the matrix has not changed),
##                 then the cachesolve retrieves the inverse from the cache.


## makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of matrix
## 4) get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        retinv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) retinv <<- inverse
        getinverse <- function() retinv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function calculates the inverse of a
## square invertible matrix created using  makeCacheMatrix function.  However,
## it first checks to see if the inverse of the matrix has already been calculated.
## If so, it gets the inverse of the matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the calculated values of
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        retinv <- x$getinverse()
        if (!is.null(retinv)) {
                message("getting cached data")
                return(retinv)
        }
        sqrinvertmat <- x$get()
        retinv <- solve(sqrinvertmat, ...)
        x$setinverse(retinv)
        retinv
}

