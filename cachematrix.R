## cachematrix.R
## 26/07/2015
## Hariharan M
 
## This file has two functions makeCacheMatrix and cacheSolve. makeCacheMatrix 
## returns a list of getter/setter functions for the matrix itself 
## and the inverse of it.
## cachemean function can be used to return the inverse of an invertible matrix
## Furthermore as the name suggests it caches the inverse of the matrix and 
## returns it when queried for the same matrix.



## This function is used to return a list of getter/setter functions that 
## for the matrix x. The list of functions are as follows
## setMatrix() - function to set the value of the matrix x
## getMatrix() - function to get the value of matrix x 
## setMatInv() - function to set the inverse of matrix x
## getMatInv() - function to get the inverse of matrix x
 
makeCacheMatrix <- function(x = matrix()) {
  
  matInv <- NULL
  
  ##setMat function definition
  setMat <- function(argMat) {
    ## Set the new matrix
    x <<- argMat
    ## Indicate the matrix value has changed
    matInv <<- NULL
  }
  
  ##getMat function definition 
  getMat <- function() x
  
  ##setSolve function definition
  setSolve <- function(Inv) matInv<<- Inv
  
  ##getSolve function definition
  getSolve <- function() matInv
  
  ## Return list of functions created above
  list(setMatrix = setMat, getMatrix = getMat,
       setMatInv = setSolve,
       getMatInv = getSolve)

}


## This function returns the inverse of the matrix x only if its created using 
## the function makeCacheMatrix. Any additional arguments passed will further 
## be passed to the solve function while creating the inverse for x.
## The function will first lookup if the matrix inverse exists within its cache
## and returns it if it exists or creates it and then stores it in the cache.

cacheSolve <- function(x, ...) {
       
    ## Get the matrix inverse to check if it already exists
    matInv <- x$getMatInv()
    
    ## check if retreived matrix inverse is NULL
    if(!is.null(matInv)){
      
      message("Getting cached data")
      
      ## Return the cached matrix inverse
      return (matInv)
    }
    
    
    mat <- x$getMatrix()
    
    ## Use solve function pass the matrix and arguments 
    matInv <- solve(mat, ...)
    
    ## Store the matrix inverse create above for future usage
    x$setMatInv(matInv)
    
    ## Return a matrix that is the inverse of 'x'
    matInv
    
}
