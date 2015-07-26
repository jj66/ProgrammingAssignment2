## Put comments here that give an overall description of what your
## functions do: It inverts a an invertible matrix, caching it and 
## avoiding to recalculate it if it was a repetition
## in that case retrieves it from cache
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) { inv = NULL  # Initializ. NULL represents the null object in R
Setjj = function(y) {
  x <<- y   # I use `<<-` to assign a value to an object in an environment different from the current environment. 
  inv <<- NULL}
get = function() x
Sinvjj = function(inverse) inv <<- inverse 
Ginvjj = function() inv # get inverse
list(Setjj=Setjj, get=get, Sinvjj=Sinvjj, Ginvjj=Ginvjj)}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {## gets the inverse of 'x'
  inv = x$Ginvjj()  # if the inverse was already calculated 
  if (!is.null(inv)){   #(is.null returns TRUE if its argument is NULL and FALSE otherwise.)
    message("caching previous results") #retrieve it from the cache and skips the computation. 
    return(inv) }  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)  # sets the value of the inverse in the cache via the solve function.
  x$Sinvjj(inv)
  return(inv)}
