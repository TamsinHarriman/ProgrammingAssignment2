## makeCacheMatrix creates a matrix that can cache its inverse, and then cacheSolve computes the
## inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from
## the cache.

## This function caches a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(m) m <<- solve(x)
	getinverse <- function() m
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)

}


## This function takes the inverse of the matrix returned by makeCacheMatrix, or retrieves
## the inverse from the cache if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
