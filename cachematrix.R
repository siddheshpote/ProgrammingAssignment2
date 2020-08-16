#create a matrix then check its inverse then check whether the inverse is already cached

## The first function, makeCacheMatrix creates a special "matrix", 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set , get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data , ...)
  x$setInverse(inv)
  inv      ## Return a matrix that is the inverse of 'x'
}

