## Two functions that work in conjunction to determine the inverse matrix of some x as well as caching
## that inverse matrix for use again should it be called immediately after

## Resets the cache and creates a list of functions to be called later
makeCacheMatrix <- function(x=matrix()){
  ## Initialize/Reset the cache globally
  imatrix<-NULL
  
  ## List of functions used by cacheSolve to retrieve, calculate the inverse, and update the cache
  get <- function()x
  setmatrix<-function(matrix) imatrix <<- matrix
  getmatrix <- function() imatrix
  
  ## Sets the list of functions up to be called
  list(get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## Checks for the cache and either returns the previous cache or calculates the new inverse matrix  
cacheSolve <- function(x,...){
  ## Retrieves the cached matrix from the makeCacheMatrix function
  imatrix<- x$getmatrix()
  
  ## Checks for data in the cache.  If there are data in the cache return data, if not, do nothing.
  if(!is.null(imatrix)){
    message("getting chached data")
    return(imatrix)
  }
  
  ## Retrieves the matrix from makeCacheMatrix, calculates the inverse, sets the cache to the inverse
  data<- x$get()
  imatrix<-solve(data,...)
  x$setmatrix(imatrix)
}
