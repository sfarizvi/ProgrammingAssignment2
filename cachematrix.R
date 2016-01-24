## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Week 3 Assignment 2 First function makeCacheMatrix
##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { ##beginning of function
  a <- NULL
  set <- function(y)  ## set changes the vector stored in the main function
  { ##beginning of set
    x <<- y
    a <<- NULL
  }  ## end of set
  get <- function() x   ##get returns the vector x stored in main funciton
  setinverse <- function(solve) a <<- solve  ##computing inverse using solve
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}  ##end of function  Author Fuzail

## Write a short comment describing this function
##Week 3 Assignment 2 Second function cacheSolve
##This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve 
##the inverse from the cache.
cacheSolve <- function(x, ...) { ## beginning of function
        ## Return a matrix that is the inverse of 'x'
  a <- x$getinverse()
  if(!is.null(a)) 
  { ##begin of if
    message("getting cached data")
    return(a)
  } ## end of if
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
  } ##end of function  Author Fuzail
