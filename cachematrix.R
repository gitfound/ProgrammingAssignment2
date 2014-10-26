## cachematrix.R contains two functions designed to
## 		reduce computation time 
## 		when solving for the inverse
## 		of large matrices
## 		by utilizing caching

## How do I use these functions?
## 		1. call makeCacheMatrix on m and assign it to an object, where m is any invertible matrix
##			mCM <- makeCacheMatrix(m)
##		2. call cacheSolve on stored object, and R will return the inverse matrix of m
##			cacheSolve(mCM)

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
##		1. set the value of the matrix
##		2. get the value of the matrix
##		3. set the value of the inverse matrix
## 		4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set=set, get=get,
		 setmatrix=setmatrix,
		 getmatrix=getmatrix)
}

## cacheSolve calculates the inverse matrix of the special "matrix" created with makeCacheMatrix; however, 
## 		it first checks to see if the inverse of the matrix has already been calculated, 
## 		if true, it gets the matrix from the cache and skips the calculation, 
## 		if false, it calculates the inverse matrix and sets the value of the matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
	## return a matrix that is the inverse of 'x'
}
