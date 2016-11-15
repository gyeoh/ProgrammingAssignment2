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

    
## testing the 2 functions above without the hashes as per below
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) 
## myMatrix_object <- makeCacheMatrix(m1)
## cacheSolve(myMatrix_object)

# returns (and caches) the matrix inverse of m1 which is equal to
# matrix(c(6,2, 8,4), nrow = 2, ncol = 2)
# check m1 %*% matrix(c(6,2, 8,4), nrow = 2, ncol = 2)
# gives the 2 by 2 identity matrix matrix(c(1,0, 0,1), nrow = 2, ncol = 2)
# doing cacheSolve(myMatrix_object) a second time should simply fetch (but not
# recalculate) the inverse of m1 (and report that it only fetched it)
