## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeavector<-function(x=numeric()){
  m=NULL
  set<-function(y){
    x<<-y
   m=NULL
  }
  get<-function()x
  setmean<-function(mean)m<<-mean
  getmean<-function()m
  list(get=get,set=set,getmean=getmean,setmean=setmean)
}

cachemean<-function(x,...){
  m<-x$getmean()
  if(!is.null(m)){
    message("getting chached data")
    return(m)
  }
 data<-x$get()
 m<-mean(data,...)
 x$setmean(m)
 m
}

makeCacheMatrix <- function(x = matrix()) {
    inv<<-NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function(){x}
    setInverse<-function(Inverse)(inv<<-inverse)
    getInverse<-function(){inv}
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}
