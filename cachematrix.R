## This is the second assignment in R Programming course.
## This program use the given example, changing mean() by solve()

## makeCacheMatrix creates a vector for setting and getting the value 
## of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x <<-y
    m <<-NULL
  } 
  get<-function()x
  setInverse <-function(inverse) m<<-inverse
  getInverse <-function() m
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}

## cacheSolve verify if the inverse is already calculated
## sending the message "getting cached inversed matrix"
## otherwise make the calculation

cacheSolve <- function(x, ...) {
  m<<-x$getInverse()
  if(!is.null(m)){
    message("getting cached inversed matrix")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setInverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
