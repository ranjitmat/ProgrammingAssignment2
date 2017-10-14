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
  setinvr<-function(inverse) invr<<-inverse
  getInvr<-function()invr
    list(set=set,get=get,setinvr=setinvr,getInvr=getInvr)
    
}


## This function computes the inverse of the Matrix created by above function. 
##If the inverse is already created then it should get from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr<-x$getInvr()
  if(!is.null(invr)){
    message("getting the details from cached data")
    return(invr)
  }
  cmptdv<-x$get()
  invr<-solve(cmptdv,...)
  x$setinvr(invr)
  invr
}
