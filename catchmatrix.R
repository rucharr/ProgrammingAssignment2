## In this assignment matrix is created using makeCacheMatrix and cacheSolve function which captures inverse of a matrix. 

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	n <- NULL
	set <- function (z) {
		x <<- z
		n <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) n <<- inverse
	getinv <- function() n
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. In order to calculate the inverse solve() function is being used.

cacheSolve <- function(x, ...) {
      n <- x$getinv()
	if(!is.null(n)) {
		message("getting cached data")
		return(n)
	}
	data <- x$get()
	n <- solve(data, ...)
	x$setinv(n)
	n
}
