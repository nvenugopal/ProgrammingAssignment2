## Put comments here that give an overall description of what your
## functions do
##
## First create a matrix. Next, this matrix can be passed into the first
## function makeCacheMatrix().  This function creates a special cached matrix 
## object and creates a list of four methods or functions that can operate
## on this object
##
## Once the special cached matrix object is created, the first call
## to the function cacheSolve() determines that the inverse for this matrix
## has not been previously calculated.  The first call creates the inverse
## and stores it in the cache by using the cached matrix object previously created.
## The next time there is a need to compute the inverse for this matrix,
## the cacheSolve matrix determines that the inverse for this already exists
## and therefore retrieves it from memory, without going through the solve 
## process of matrix inversion.
## It is important to note that, if the matrix has changed, then the 
## cached matrix object needs to be updated, and a new inverse will need
## to be calculated.  
##
## One can use the identical(mat1, mat2) function to compare if the two
## matrices are identical.  If so, the inverse for mat1 can be used from
## cache.


## Write a short comment describing the function makeCacheMatrix()

## This function creates a special "matrix" object and a list  
## containing functions that do the following:
## 1.  Sets the values of the square matrix
## 2.  Gets the values of the square matrix
## 3.  Sets the inverse of the matrix
## 4.  Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse matrix to be all NULLs
  minv <- NULL
  
  ## Function below will store the matrix passed into
  ## the function, into a special cached matrix object.
  ## Since x has just been created, its inverse "minv" is set to NULL
  set <- function(y) {
    ## matrix data passed into the function is stored
    ## in an environment that is different from the current
    ## environment, namely in the cache.
    x <<- y
    minv <<- NULL
  }
  
  ## The get function simply returns the matrix x
  get <- function() x
  
  ## The setinverse function invokes the solve function
  ## to compute and store the inverse of the input matrix
  ## into the matrix "minv"
  setinverse <- function(solve) minv <<- solve
  
  ## getinverse returns the inverse matrix stored in m
  getinverse <- function() minv
  
  ## The list below is a list of functions that were described 
  ## previously
  list (set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve will return the inverse of a matrix, if
## it already exists in the cache; if not it will compute
## it, store it in the cache and return the computed inverse
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinverse()
  if(!is.null(minv)) {
    ## A inverse exists for this matrix in the cached data
    message("getting cached data")
    return(minv)
  }
  ## inverse does not exist in the cache
  ## Get the matrix data from cache
  data <- x$get()
  
  ## Compute the inverse of the matrix
  minv <- solve(data, ...)
  
  ## Set the inverse into the cache, associated with the matrix object
  ## for future retrieval
  x$setinverse(minv)
  
  ## Return the inverse
  minv
}
