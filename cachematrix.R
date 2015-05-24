## Put comments here that give an overall description of what your
## functions do

## Matrix inverse is usually a time consuming process
## The major purpose of this algorithm is to cache the inverse of matrix 
## without time costly computation (avoid repeat calculating process)




## Write a short comment describing this function

##function description
##function (makeCacheMatrix) is created to set up the value of matrix and return the value of matrix 
##set up the value of inverse matrix and return the value of inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
  
  invorigin <- NULL
  setup<- function(y) {
    x <<- y
    invorigin <<- NULL
  }
  func <- function() x
  calfunc <- function(inverse) invorigin <<- inverse
  retfunc <- function() invorigin
  list(setup=setup, func=func, calfunc=calfunc, retfunc=retfunc)

}


## Write a short comment describing this function
##function (cacheSolve) is created to return the inverse result without time costing computation. 
##if there is no inverse result, function (cacheSolve) will automatically calculate the inverse
##and return the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invorigin <- x$retfunc()
  if(!is.null(invorigin)) {
    message("getting cached data.")
    return(invorigin)
  }
  data <- x$func()
  invorigin <- solve(data)
  x$calfunc(invorigin)
  invorigin
}



## create a sample x and run from the above functions
x<-rbind(c(100, 60), c(85, 10))
m<-makeCacheMatrix(x)
## get matrix 
m$func()
## get inverse matrix
cacheSolve(m)
