## These two functions are used to create objects that store a 
## invertible matrix and cache's its inverse

## The makeCacheMatrix function takes a invertible matrix as a parameter 
## and returns a special "matrix" object that is a list of four funcitons
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
    setinverse = setinverse, getinverse = getinverse)
}


## The CacheSolve function calculates the inverse of the special "matrix"
## created by the makeCacheMatrix function
## It is assumed that the matrix is square and invertible
## If the inverse is already calculated the cached value is returned 
## If the inverse is not present, it is calculated, cached and returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  identity <- diag(dim(matrix)[1])
  i <- solve(matrix, identity, ...)
  x$setinverse(i)
  i
}


