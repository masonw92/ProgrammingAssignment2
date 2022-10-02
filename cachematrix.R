## These functions are meant to calculate a matrix's inverse
## They also do a search to see if an inverse is already calculated
## After being ran the inverse is returned for a matrix

## This function creates a matrix then caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function()x
  setinv <- function(inverse)inv<<-inverse
  getinv <- function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function searches for a cached inverse of a given matrix, 
## if one doesn't exist then one is calculated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
