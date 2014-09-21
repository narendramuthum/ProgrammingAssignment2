# Below steps for create 'makeCacheMatrix' function.
# Creates a matrix object that can cache its inverse.
# The object doesn't calculate the inverse, just saves it inside.
# Saves the matrix to variable x and its inverse in to variable m in scope.
# Returned object (actually it is a list) contains methods:
# set: sets vector and resets cached mean
# get: returns vector values
# setmean : saves solve mean value
# getmean : returns cached mean value

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}


## Sample run1:
>x <- matrix(c(1,2,3,4),nrow=2,ncol=2)
>m1 = makeCacheMatrix(x)
>m1$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
## No cache in the first run
>cacheSolve(m1)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
## Retrieving from the cache in the second run
>cacheSolve(m1)
getting cached data
getting cached data.
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


## Sample run2:
>x <- matrix(c(1,3,5,6,2,8,7,9,4),nrow=3,ncol=3)
>m1 = makeCacheMatrix(x)
>m1$get()
     [,1] [,2] [,3]
[1,]    1    6    7
[2,]    3    2    9
[3,]    5    8    4

## No cache in the first run
>cacheSolve(m1)
            [,1]        [,2]        [,3]
[1,] -0.27586207  0.13793103  0.17241379
[2,]  0.14224138 -0.13362069  0.05172414
[3,]  0.06034483  0.09482759 -0.06896552

## Retrieving from the cache in the second run
>cacheSolve(m1)
getting cached data.
            [,1]        [,2]        [,3]
[1,] -0.27586207  0.13793103  0.17241379
[2,]  0.14224138 -0.13362069  0.05172414
[3,]  0.06034483  0.09482759 -0.06896552



