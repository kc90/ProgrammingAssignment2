#This function basically creates a list of 4 functions:
#set,get,setinv,getinv.
#Working Principle:
#A matrix is passed as an argument;It's Inverse is to be
#calculated but for the sake of efficiency,there is a check
#conducted to see if the inverse already exists in the Cache
#or not and if computation is necessary.
#The functions are explained below.

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL#The inverse is set to NULL
  set=function(y){#This func sets the matrix passed as arg to makeCacheMatrix function
    x<<-y
    inv<<-NULL# sets inv also to NULL
  }
  get=function() x # func to get and display matrix
  setinv= function(inver) inv<<-inver#func to set inverse passed as arg to inv 
  getinv= function() inv #display inverse
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)#list containing all 4 func
}


## This part of the working principle basically checks 
#to see if the inverse is already available in Cache
#If it is available(NOT NULL),it is returned else 
#the inverse is computed as shown below.

cacheSolve <- function(x, ...) {
  inv=x$getinv()#calls func to get value of inverse
  if (!is.null(inv)){#checks to see if it is not null
    message("getting cached data")
    return(inv)
  }
  data=x$get()#If NULL,then the matrix is obtained to data variable
  inv= solve(data)#solve function used to calc inverse
  x$setinv(inv)#computed inverse and passed to the func as arg
  inv
}
