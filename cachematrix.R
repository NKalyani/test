#Caching the inverse of a matrix:
#makeCacheMatrix is a function used to create a matrix
#Matrix inversion is usually a costly computation and there may be some benefit to 
#caching the inverse of a matrix rather than compute it repeatedly.
#Below are a  pair of functions to craete a matrix and cache its inverse 


makeCacheMatrix <- function(x = matrix()) {
  INV<-NULL
  set<-function(y){
    x<<-y
    INV<-NULL
  }
  get <- function ()x
  setInverse<-function(inverse) INV<<-inverse
  getInverse<-function() INV
  list(set= set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}




#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  INV<-x$getInverse()
  if(!is.null(INV)){
    message("getting cached data")
    return(INV)
  }
  mat<-x$get()
  INV<-solve(mat,...)
  x$setInverse(INV)
  INV
}
