## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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