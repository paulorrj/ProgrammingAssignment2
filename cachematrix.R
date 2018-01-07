## The idea of this project is to save computer processing. 
## Matrix inversion is a costly calculation, so it makes sense 
## to reuse results if you've already done that specific computing.
## The combination of these two functions allows for that.

## This function takes a matrix and returns a structure to be used
## by the cacheSolve function. The structure store a matrix and its
## inverse. The return is a list consisting on 4 functions and
## 2 objects

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, 
           getinverse = getinverse)

}


## chacheSolve is designed to populate or retrive the Inversion of an
## object of the type makeCacheMatrix, which is, in itself, 
## a structured version of the matrix we want to invert.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}
