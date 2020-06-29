## This script provides two functions makeCacheMatrix() and cacheSolve() 
## that cache the inverse of a matrix. 

## The first function, makeCacheMatrix() creates a special "matrix", 
## which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) k <<- inverse
  getinverse <- function() k
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  k <- x$getinverse()
  if (!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinverse(k)
  k
}

## Testing the function

mat <- matrix(1:4, 2, 2)

mcm <- makeCacheMatrix(mat)

mcm$get()

mcm$getinverse()

cacheSolve(mcm)

