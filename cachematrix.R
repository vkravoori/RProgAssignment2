## Cache inverse of a given matrix
## This function assumes that the matrix is invertible
## Caching helps as it is readily available and as computation of Inverse matrix becomes complex based ## on dimension and if there is a need for using the inverse in multiple programs/function, it is advisable ## to cache.

makeCacheMatrix <- function(x = matrix()) {
  
  ## inv  is set to NULL, initializing it as an object within the makeCacheMatrix() environment to be used ##            by later code in the function.
  inv <- NULL   
  
  ## Defining set function 
  ## <<- which assigns the value of y to x in the parent environment, when set function is executed
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Defining get function
  
  get <- function() x
  ## Define setInverse and getInverse
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cachSolve function calculates the inverse of the matrix created by makeCachMatrix function
## when it is executed, if the inverse has already been calculated, then it simply retrieves from the cache

cacheSolve <- function(x, ...) {
  ## when executed, returns the inverse of the matrix x 
  inv <- x$getInverse()
  ## if inverse exists, is not null, then get cached data
  if (!is.null(inv)) {
    message("return inverse matrix from the cache")   
    return(inv)
  }
  matrixx <- x$get()
  inv <- solve(matrixx, ...)
  x$setInverse(inv)
  inv
}


