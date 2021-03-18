## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates an object to hold a matrix
## and (eventually) its inverse
makeCacheMatrix <- function(x = matrix()) {
  #Initialize the inverse 
  inverse_matrix <- NULL
  
  #Setting the matrix
  set<-function(matrix){
    m <<- matrix
    inverse_matrix <<- NULL
  }
  
  #Getting the matrix
  get<-function() x
  setinverse_m<-function(inverse) {inverse_matrix <<- inverse}
  getinverse_m<-function() {inverse_matrix}
  
  list(set=set, get=get, setinverse_m=setinverse_m ,getinverse_m=getinverse_m)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix stored
## in the 'makeCachMatrix' structure. Further calls to cacheSolve()
## will return a cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cached <- x$getinverse_m()
    
    if(!is.null(cached)){
      return(cached)
    }
    
    m<- x$get()
    cached <- solve(m)
    x$setinverse_m(cached)
    cached
}
