## makeCacheMatrix creates a special "matrix" object that can cache its inverse

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  mo <- NULL                                
  set<-function(y){                            
    x <<- y                             
    mo <<- NULL                         
}
  
  getmatrix <- function() x
  
  setinverse <- function(solve) mo <<- solve
  
  getinverse <- function() mo
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x = matrix(), ...) {
  mo <- x$getinverse()
  
  if(!is.null(mo)){
    message("Retrieving data from cache")
    return(mo)
  }
  
  matrix <- x$getmatrix()
  
  mo <- solve(matrix, ...)
  
  x$setinverse(mo)
  mo
}
