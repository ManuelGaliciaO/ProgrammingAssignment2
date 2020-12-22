## The functions makeCacheMatrix and cacheSolve are meant to work together
## in creating a matrix object and computing its inverse, the inverse is then
## cached for later retrieval.

## Function initializes the values of the matrix and the cache, it also sets up 
## all the functions/pointers that cacheSolve will need to solve the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    
      x <<- y
      m <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  ##The list allows us to reference the functions from within cacheSolve
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 

}


## Function retrieves matrix created in makeCacheMatrix and solves it to get
## the inverse matrix, then it returns it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()

## If the cached value is different to "NULL" then we return/display the value
## of "m", otherwise we compute the inverse matrix
  
  if(!is.null(m)) {
    
      message("getting cached data")
      return(m)

  }
  matrix <- x$get()
  m <- solve(matrix)
  x$setInverse(m)
  m
}
