## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## This function creates the Cache Matrix vector containing functions for the following :
# 1Setting the value of the matrix
# 2 Getting the value of the matrix
# 3 Setting the value of inverse of the matrix
# 4 Getting the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix<-NULL
  set<-function(y) {
  x<<-y
invMatrix<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) invMatrix<<- inverse
  getinverse<-function() invMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##The function below will create an inverse of a matrix. First it will check if the inverse alraedy exists, if it does it will 
## provide the message and get the inverse from the cache itself. Else it will calculate.


cacheSolve <- function(x, ...) {
invMatrix<-x$getinverse()
  if(!is.null(invMatrix)) {
    message("Data Available in Cache")
    return(invMatrix)
  }
  data<-x$get()
  invMatrix<-solve(data)
  x$setinverse(invMatrix)
  invMatrix
}
