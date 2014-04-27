## R Programming
## John Hopkins University
## Coursera, April 2014
## 2 programming assignment. Global variables. Assignment: Caching the Inverse of a Matrix
## Script by Yaroslav Shvets

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
#Two simple verifications are performed first: we are expecting to get an invertable matrix as
#the argument for makeCacheMatrix() function. So we check whether the matrix is square and its 
#determinant is not equal to zero. A warning is shown if one of this conditions is false.
  
  if (nrow(x)!=ncol(x))
    print ("Warning! The matrix you've entered is not square. It cannot be inverted")
  else
    if (det(x)==0)
      print ("Warning! The determinan of the matrix is zero. It cannot be inverted")

#The rest of the funcion is almost unchanged

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


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

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
