## Put comments here that give an overall description of what your
## functions do

## Below written makeCacheMatrix function creates a matrix object... 
##...to cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  invr=NULL
  set<-function(y){
    x<<-y
    invr<<-NULL
    
  }
  get<-function()x
  setInverse<-function(inverse) invr<<-inverse
  getInverse<-function()invr
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function computes the inverse of the Matrix created by above function. 
##If the inverse is already created then it should get from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr<-x$getInverse()
  if(!is.null(invr)){
    message("getting from cached data")
    return(invr)
  }
  mat<-x$get()
  invr<-solve(mat,...)
  x$setInverse(invr)
  invr
}
