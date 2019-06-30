#################################################################################################
#R Programming Course
#Programming Assignment - 2
#
#Write the following functions:
#1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#2. cacheSolve: This function computes the inverse of the special "matrix" returned by 
#    makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
#    changed), then the cachesolve should retrieve the inverse from the cache.
#
#Computing the inverse of a square matrix can be done with the solve function in R. For example, 
#  if X is a square invertible matrix, then solve(X) returns its inverse.
#For this assignment, assume that the matrix supplied is always invertible.
#################################################################################################

makeCacheMatrix <- function(x = matrix()) {
        #initialize
        inv <- NULL
        #Cache - set value of matrix to new matrix and reset the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get value of matrix from cache
        get <- function() x
        #set inverse of matrix
        setinverse <- function(inverse) inv <<- inverse
        #get inverse of matrix from cache
        getinverse <- function() inv
        #special vector 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x) {
        #get value of inverse from cache
        inv <- x$getinverse()
        #use cached value if exists
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        #if not in cache then calculate new
        data <- x$get()
        #calculate inverse
        inv <- solve(data)
        #set inverse value to cache
        x$setinverse(inv)
        inv
}

###test the code
#m<-matrix(c(1,2,3,4),nrow=2,ncol=2)
#m<-matrix(c(3,4,5,9),nrow=2,ncol=2)
#testvec<-makeCacheMatrix(m)
#cacheSolve(testvec)
#testvec$get()
#testvec$getinverse()
#testvec$setinverse(matrix(c(5,6,7,8),nrow=2,ncol=2))
#cacheSolve(testvec)
#testvec$getinverse()

