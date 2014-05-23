##These are two functions that work in pair
##to compute the inverse of a matrix and
##cache the result , in case we need to use it again
##and skip the time-consuming computations

##makeCacheMatrix creates a special "matrix",
##which contains the appropriate functions to
##set and get the value of the matrix
##and the value of the inverse matrix as well.

makeCacheMatrix <- function(x = matrix()) {
 
  m<- NULL
  
  set<-function(y){
    x<<-y
    m<<-NULL
    }
  
  get<-function() x
  setInverse<- function(solve) m<<-solve
  getInverse<- function() m 
  
  list(set=set, get=get , 
       setInverse=setInverse, 
       getInverse=getInverse)
  
}

##cacheSolve computes the inverse of the matrix 
##and places it in cache.
##If the inverse matrix has been already calculated,
##it retrieves it from the cache.

cacheSolve <- function(x, ...) {
  
  m<-x$getInverse()
 
   
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
  
   data <-x$get()
   m<-solve(data, ...)
   x$setInverse(m)
   m
 }
