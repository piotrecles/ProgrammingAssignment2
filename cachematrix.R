## This is pretty much line the example, the only real
## differences being the input to the first function
## is a matrix, and we're using solve rather than mean

## This is the state function thing.  It uses 'solve'
## for the actual computation.  The assignment said
## to assume the matrix was invertible, so we don't have
## to check for that, and we don't.
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function()x
  
  set_inverse<-function(solve)inverse<<-solve
  get_inverse<-function()inverse
  
  list(set=set,get=get,
       set_inverse=set_inverse,get_inverse=get_inverse)
}

## Returns a matrix that is the inverse of 'x'
## The input into this is the fancy pants list above
## made with the matrix you want
cacheSolve <- function(x, ...) {
  inverse<-x$get_inverse()
  if (!is.null(inverse)){
    message("getting cached inverse")
    return(inverse)
  }
  data<-x$get()
  
  ## the assignment allowed us to assume an inverse exists
  inverse<-solve(data,...)
  x$set_inverse(inverse)
  inverse
}
