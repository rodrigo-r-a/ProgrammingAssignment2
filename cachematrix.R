#makeCacheMatrix function is the one in charge of creating the matrix.
#this function sets the value for x, then gets that value. It will after set the values for the inverse of the matrix that will 
#be created using either cbind or rbind.

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The following portion of the code will compute the result of the inverse matrix that has been calculated before.

cacheinvr <- function(x, ...) {
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$setinverse(invr)
  invr
}
#example
x = rbind(c(5, 9), c(.5, .5))
k = makeCacheMatrix(x)
k$get()
#  [,1] [,2]
#[1,]  5.0  9.0
#[2,]  0.5  0.5
cacheinvr(k)
#      [,1] [,2]
#[1,] -0.25  4.5
#[2,]  0.25 -2.5
