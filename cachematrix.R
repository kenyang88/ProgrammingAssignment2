## create a special "matrix" object that stores a matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  matrix_inversed <- NULL
  set <- function(y){
    x <<- y
    matrix_inversed <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {matrix_inversed <<- inverse}
  getInverse <- function() {matrix_inversed}
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed),
## retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inversed_matrix <- x$getInverse()
  if(!is.null(inversed_matrix)){
    message("getting cached data")
    return(inversed_matrix)
  }
  
  y <- x$get()
  
  inversed_matrix <- solve(y, ...)
  
  x$setInverse(inversed_matrix)
  
  inversed_matrix
}

