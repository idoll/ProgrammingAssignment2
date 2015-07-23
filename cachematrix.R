## Put comments here that give an overall description of what your
## functions do

## This function Caches a matrix which is passed in. It is comprised of 4 helper functions set, get, setmatrix, and getmatix.
##These functions help perform the requires actions to properly cache and retrieve the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) i <<- inverse
  getmatrix <- function() i
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function determines if the matrix 'X' which has been passed in is already cached and if it is returns the cached inverse of the matrix.
## If it is not already cached it will inverse the matrix and store it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getmatrix()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  i = solve(x$get())
  x$setmatrix(i)
  i
}
