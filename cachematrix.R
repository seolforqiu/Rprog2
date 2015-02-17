## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse matrix to NULL
  inverse <- NULL
  
  ## set function for setting the matrix x
  set <- function(y){
  	## set matrix x to y
    x <<- y
    ## set the inverse matrix to NULL
    inverse <<- NULL
  }
  
  ## get function for getting the matrix x 
  get <- function() x
  
  ## set the inverse matrix to inverseM
  setInv <- function(inverseM) inverse <<- inverseM
  
  ## get the inverse matrix
  getInv <- function() inverse
  
  ## return a list of functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## pulling the inverse matrix from x and assign to inverse
  inverse <- x$getInv()
  
  ## if inverse is not NULL, which indicates resolving is not necessary, then return inverse
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  ## if inverse is NULL, fetch the matrix itself
  data <- x$get()
  
  ## solve for the inverse of that matrix data, set it to the inverse variable in x and return it
  inverse <- solve(data, ...) 
  x$setInv(inverse)
  inverse
}
