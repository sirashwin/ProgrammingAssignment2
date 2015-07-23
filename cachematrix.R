## The following R code is for storing the inverse of a matrix in a cache,
## so whenever we want to find the inverse it can be retrieved from the cache.
##
## If the inverse is being calculated for the first time or the the matrix has
## altered, we calculte the inverse and store it in the cache.





## The first function, makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to -
##
##            1) set the value of the matrix
##            2) get the value of the matrix
##            3) set the value of the inverse of the matrix
##            4) get the value of the inverse of the matrix
##
## As you may notice the logic used is similar to that used in makeVector function
## in the example given in the question. The word "mean" has been replaced by
## "inverse" appropriately.
## Also note, that the argument 'x' the function inputs is of the matrix class,
## not numeric.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) m <<- inverse
       getinverse <- function() m
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}





## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the setinverse function.
##
## As you may notice the logic used is similar to that used in cachemean function
## in the example given in the question.
## Note that here we have to use the solve function, instead of the mean
## function.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
