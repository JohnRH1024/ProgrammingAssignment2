## Matrix inversion can be very time consuming, especially for large matrices.
## If a large matrix has to be inverted multiple times for some particular program
##  it makes sense to cache it, or store a copy that can be retreived quickly
##  in place of the expensive computation.


## This function creates a special "matrix" object that can cache its inverse.
## The matrix and its inverse (once it is calculated) is stored in a cache using the 
##   operator <<- that assigns the value to a variable in the parent environment.
## It returns a list of functions that get/set the value of the matrix and its 
##   inverse within a particular environment.

makeCacheMatrix <- function(x = matrix()) {
	mi <- NULL
	set <- function(y) {
		x <<- y
		mi <<- NULL
	}
	get <- function() x
	setinv <- function(inv) mi <<- inv
	getinv <- function() mi
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function uses the list of functions created in makeCacheMatrix.
## For the particular environment in which the functions were defined, 
##  this first looks to see if the inverse of this matrix has already been
##  calculated and saved/cached.  If it has been, then the cached value is
##  returned.
## If the inverse of this particular matrix has not been calculated yet,  then the
##  matrix to be inverted is retrived from the cache and the function 'solve'
##  is used to computer the inverse which is then stored in the cache

cacheSolve <- function(x, ...) {
	mi <- x$getinv()
	if(!is.null(mi)) {
		message("getting cached inverse")
		return(mi)
	}
  
	data <- x$get()
	mi <- solve(data, ...)
	x$setinv(mi)
	mi
}
