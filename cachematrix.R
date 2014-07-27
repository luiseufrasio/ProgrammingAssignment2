## The first function creates the structure to cache a matrix and its inverse
## The second function builds the inverse of a matrix m if its not builded yet.
## Otherwise it reuses the inverse created before.

## Creates a cached matrix using <<- operator
makeCacheMatrix <- function(m = matrix()) {
	i <- NULL
	set <- function(n) {
		m <<- n
		i <<- NULL
	}
	get <- function() m
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## Return a matrix that is the inverse of 'm'
cacheSolve <- function(m) {
	i <- m$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- m$get()
	i <- solve(data)
	m$setinverse(i)
	i
}