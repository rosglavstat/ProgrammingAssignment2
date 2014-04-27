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

  m <- NULL #set m variable to NULL
  set <- function(y) {  #set() function is used to set the values of the matrix that is going to be inverted
    x <<- y
    m <<- NULL
  }
  get <- function() x #get() function just types the matrix
  setinverse <- function(inverse) m <<- inverse #setinverse() function gets the argument "inverse" and sets it to global variable m
  getinverse <- function() m #getinverse() function prints the value of m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #calls the getinverse() function for the special "matrix" returned by makeCacheMatrix
  if(!is.null(m)) { #if it is the first time the function is called, the inverse is not calculated yet
                    #and is equal to NULL, if it was calculated before it is not NULL
    message("getting cached data") #notification
    return(m) # return the pre-calculated value of m (inverted matrix) and ignore the code below
  }
#if m is equal to NULL (first call)
  data <- x$get() # get the matrix
  m <- solve(data, ...) #invert the matrix
  x$setinverse(m) #call the setinverse() function to cache the inverse matrix for further calls
  m  #print the inverted matrix
}
