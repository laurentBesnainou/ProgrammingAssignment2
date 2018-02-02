## Function to define Matrix inversion with caching the inverse of a matrix rather
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather 
# than compute it repeatedly

## this function creates a special "matrix" object that can cache its inverse
###########################################
#### you have to install MASS package in RStrudio > go to package and add it to test the code
###########################################

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  #Two values are stored, the Matrix and its inverse
  matrix <- NULL
  inverse <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    matrix <<- NULL
    invesre <- NULL
  }
  
  setinverse <- function(matrix, inverse) {
    inverse <<- inverse
    matrix <<- matrix
  }
  getinverse <- function() inverse
  getmatrix <- function() matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix (and the matrix has not changed)
## the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #browser()
  inv <- x$getinverse()
  mat <- x$getmatrix()
  if(!is.null(inv)) {
    #check if matrix are identicals before returninv inverse of matrix
    if (identical(x$get(),mat )) {
    print("getting cached data")
    return(inv)
    }
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinverse(data,m)
  m
  
}

#Utilisation
m <- matrix(c(1:10),2,5)
mDiff <- matrix(c(1:9,11),2,5)
m1 <- makeCacheMatrix(m)
m2 <- makeCacheMatrix(mDiff)
cacheSolve(m1)
cacheSolve(mDiff)
