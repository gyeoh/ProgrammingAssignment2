## Lexical scoping is used to retrieve values from objects based on the way 
## functions are nested when they were written. 

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function (y) {
    x <<- y ## Assign the input argument y to the x object in the parent environment,
    mx <<- NULL ## Assign the value of NULL to the m object in the parent environment.
  }
  get <- function() x ##print x
  setinversematrix <- function(inverse) mx <<- inverse ## inversing matrix mx
  getinversematrix <- function() mx
  list(set = set, ## gives the name 'set' to the set() function defined above
       get = get, ## gives the name 'get' to the get() function defined above
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}  


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  mx <- x$getinversematrix()
  if(!is.null(mx)) {
    message("getting cached inversed matrix")
    return(mx)
  }
  data_inversematrix <- x$get()
  mx <- solve(data_inversematrix)
  x$setinversematrix(mx)
  mx
}
