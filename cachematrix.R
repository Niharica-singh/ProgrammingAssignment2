# makeCacheMatrix creates a list containing a function to
# set and get value of a matrix
# value of inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# Inverse of the matrix returned by makeCacheMatrix
# If the inverse has already been calculated then gets results and skips computation
# if not, computes inverse using setinverse function
# function assumes matrix is always invertible

cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data.")
return(inv)
}
data <- x$get()
inv <- solve(data)
x$setinverse(inv)
inv
}

## Run sample
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
##> m$get()
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00
##> cacheSolve(m)
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667
