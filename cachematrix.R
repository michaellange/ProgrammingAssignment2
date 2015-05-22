## Two functions for the capability to cache the inverse of a matrix.
##
## Usage: 1. Create a cacheMatrix with the function makeCacheMatrix()
##             e.g. my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##
##        2. Get the inverse with cacheSolve()
##             e.g. my_matrix_inv <- cacheSolve(my_matrix)
##

## Function to create a cacheMatrix, usage see above
makeCacheMatrix <- function(x = matrix()) {
	# Init cache
	matrix_inverse <- NULL

	# Setter for matrix
	set <- function(new_matrix) {
		x <<- new_matrix
		matrix_inverse <<- NULL
	}

	# Getter for matrix
	get <- function() {
		x
	}

	# Setter for the inverse to the cache
	setInverse <- function(new_matrix_inverse) {
		matrix_inverse <<- new_matrix_inverse
	}

	# Getter for the inverse from the cache
	getInverse <- function() {
		matrix_inverse
	}

	# Created object
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function to compute the inverse of a cacheMatrix, usage see above
## Remark: Additional arguments are not taken into account when it comes from the cache
cacheSolve <- function(x, ...) {
        # Read cache
	inverse <- x$getInverse()

	# Check is cache available
	if (!is.null(inverse)) {
		message("Getting inverse from cache")
		return(inverse)
	}

	# Read matrix
	matrix_data <- x$get()

	# Compute inverse
	inverse <- solve(matrix_data, ...)

	# Update cache
	x$setInverse(inverse)

	# Return result
	inverse
}
