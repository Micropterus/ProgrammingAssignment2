## make special "matrix" object consisting of list of functions able to cache inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){ ##"<<-" is superassignment operator
    x<<-y
    m<<-null
  }
  get<-function() x
  setinverse<-function(solve) m<<-solve
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## calculate inverse of matrix object created above and cache it, or return cached value if inverse already calculated/cached

cachesolve <- function(x, ...){
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data") #return this message above retrieved cached results if cached inverse already exists
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}
