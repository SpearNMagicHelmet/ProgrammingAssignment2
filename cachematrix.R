############################
## Make Cache Matrix Data Homework 
## Data Science Specialty
## Author:  Simone Kusz
## Date:  26 November, 2018
##
## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
##

## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function (y){
		x <<- y
		minv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) minv <<- inverse
	getinverse <- function() minv
	list (set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse) 
}


## Computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	minv <- x$getinverse()
	if(!is.null(minv)){
		return(minv)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(minv)
	minv
}
