## This code provides two functions demonstrating the ability of R
## to use lexical scoping to cache potentially time-consuming computations.
## The functions provided cache the inverse of a matrix.

## Usage example:
## 
## orig_matr <- matrix(c(1,2,3,4,5,6,7,8,10),3,3) 
## matr <- makeCacheMatrix()
## matr$set(orig_matr)
## cacheSolve(matr)
## round(matr$get() %*% matr$getinv(),5) ## Check if the result of matrices multiplication is an identity matrix
## cacheSolve(matr) ## Please see that the matrix is retrieved from cache


## Funciton makeCacheMatrix creates a special "vector", 
## which is really a list containing the functions to:
## 1. set the value of the matrix (and reset a precalculated inverse matrix if 
## it's different from the stored matrix)
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                ## Check if the new matrix and the old one are identical
                ## If yes, just skip the reassignment to save time on 
                ## inverse matrix calculations
                
                if(!identical(x,y)) {
                        x <<- y
                        im <<- NULL
                }
        }
        get <- function() x
        setinv <- function(inv) im <<- inv
        getinv <- function() im
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix stored by the special "vector" 
## created with the above function. However, it first checks to see if the inverse 
## matrix has already been calculated. If so, it gets the mean from the cache and skips 
## the computation. Otherwise, it calculates the mean of the data and sets the
## value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        im <- x$getinv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        ## For this assignment we assume that the matrix supplied is always invertible
        ##
        ## Otherwise we can first check if the matrix is invertible
        ## if(ncol(matrix)==nrow(matrix) && det(matrix)!=0) {
        ##      im <- solve(data,...) }
        ## else { 
        ##      im <- NULL 
        ##      message("Matrix provided is not invertible") }
        
        im <- solve(data, ...)
        x$setinv(im)
}
