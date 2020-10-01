## We will create two functions that cache a special "matrix", respectively
## compute the inverse of the given matrix, also checking if the inverse
## has already been computed prior.

## We first construct the function makeCacheMatrix which 
## creates the special "matrix"
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
	set <- function(y) {   # (1) This sets the value of our matrix
		x <<- y
		inv <<- NULL }
	get <- function() x	# (2) This gets the value of our matrix
	setinverse <- function(inverse) inv <<- inverse # (3) Set the value of the inverse
	getinverse <- function() inv	# (4) Get the value of the inverse
	list( set = set, get = get, 
	setinverse = setinverse
	getinverse = getinverse)
}
## By now, we have completed all 4 steps
## our initial function was meant to fulfill


## We'll now create the second function, cacheSolve
## whose purpose is to compute the inverse of our special "matrix",
## whilst also checking if it hasn't already been computed.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
		if(!is.null(inv)) {
			message("Getting cached data") # This is in case it was already cached
			return(inv)	}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
		inv
        ## Return a matrix that is the inverse of 'x'
}
