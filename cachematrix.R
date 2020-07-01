## Put comments here that give an overall description of what your
## functions do

## In this project, I have created two functions "makeCacheMatrix" and 
## "cacheSolve" that will cache the inverse of a given matrix. 


## The function "makeCacheMatrix" creates a special matrix object that can cache
## the inverse of the matrix. The function contains a list of functions that can 
## set the matrix, get the matrix, set the inverse, and get the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y){
                x <<- y
                i <<- NULL 
        }
        get <- function() x 
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i 
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}

## The function"cacheSolve" calculates the inverse of the matrix, but before it 
## calculates, it checks to see if the inverse has already been calculated. 
## If it already has been calculated, it gets the inverse already stored. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

