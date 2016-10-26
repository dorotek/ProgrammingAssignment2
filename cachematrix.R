## Below functions are made for computing the inverse of a square matrix, where cache is used whenever an identical matrix had been passed as an argument and a cache stores an inverted matrix already

## Function makeCacheMatrix creates a list of functions for setting the value of the vector, getting the value of the vector, setting the value of the inversion and getting the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  
  get<-function() x
  setinv<-function(inv) i <<-inv
  getinv <- function() i
  list(set=set,get=get,setinv=setinv, getinv=getinv)
}


## The below function calculates the inversion of the matrix passed in the first function above. Firstly, it checks the cached variable and if it is not null then there is already a value of inverted matrix to be returned. In that case the code does not calculate it but takes from cache. Otherwise it runs full code and inverts the matrix using a solve function, ending with storing the result in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data<-x$get()
  i<-solve(data,...)
  x$setinv(i)
  i
}