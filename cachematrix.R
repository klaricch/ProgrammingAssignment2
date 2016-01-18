#The two functions below are used to create a special object that stores a matrix and caches its inverse

#The below function creates a special "matrix" to set the value of the matrix, 
#get the value of the matrix, set the value of the inverse matrix, 
#and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inserve <- NULL
  set <- function(y) {
    x <<- y
    inserve <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) inserve <<- solve
  getSolve <- function() inserve
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

# The following function generates the inverse of a matrix created with the above function (makeCacheMatrix). 
# It checks to see if the inverse has already been calculated and if so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setSolve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getSolve()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setSolve(inverse)
  inverse
}
