## a pair of functions to calculate and/or cache the inverse of a matrix. 

## makeCacheMatrix:
##
## creates a list object containing a given matrix 'x'
## a function to calculate the inverse of the matrix
## a variable to store the calculated inverse 
## and a function to return the inverse if already calculated.

## here we use solve(a, b, ...)
## Arguments
## a == square numeric or complex matrix containing the coefficients of the linear system. Logical matrices are coerced to numeric.
## b ==	a numeric or complex vector or matrix giving the right-hand side(s) of the linear system.
## (KEY PART) If missing, b is taken to be an identity matrix and solve will return the inverse of a.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	get <- function() x
	setInverse <- function(b) inv <<- b
	getInverse <- function() inv
	list( get = get,
				setInverse = setInverse,
				getInverse = getInverse)
}

## cacheInverse:
## 
## when given a list of the type created by makeCacheInverse this function will
## call the getInverse function and return the cached inverse if it exists
## if not, it will compute the inverse,
## call the setInverse function,
## and then return the inv.

cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
				inv <- x$getInverse()
				if(!is.null(inv)){
					message("getting cached data")
					return(inv)
				}
				data <- x$get()
				inv <- solve(data, ...)
				x$setInverse(inv)
				inv
}

