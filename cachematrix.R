## Functions to create and use inverted matrices with caching ability

## Creates a cacheable matrix to input into cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x)){
    print("Please input a matrix.")
  }
  
  inverse.matrix<-NULL
  
  set<-function(y){
    x<<-y
    inverse.matrix<<-NULL
  }
  
  get<-function()x
  
  set.inverse<-function(solve) inverse.matrix<<-solve
  get.inverse<-function() inverse.matrix
  
  list(set=set,
       get=get,
       set.inverse=set.inverse,
       get.inverse=get.inverse)
}


## Calculates inverse of cacheable matrix which is returned by makeCacheMatrix()
## If inverse has already been calculated and the matrix is unchanged, cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
  inverse.matrix<-x$get.inverse
  
  if(!is.null(inverse.matrix)){
    message("Gettng cached inverse matrix")
    return(inverse.matrix)
  }
  
  matrix.to.inverse<-x$get()
  inverse.matrix<-solve(matrix.to.inverse)
  x$set.inverse(inverse.matrix)
  inverse.matrix
  
}