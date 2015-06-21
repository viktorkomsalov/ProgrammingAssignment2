## This set of functions is meant to compute the inverse of a matrix
## and reuse it from the cache if it already been computed to save time.

## The first function creates a saves the matrix in multiple
## environments and creates a list for the inverse that can be
## reused instead of being computed if the same matrix' inverse
## is requested again.

makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
	set <- function(y) {
		x <<- y
		I <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) I <<- inverse
	getinverse <- function() I
	list(set = set,
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## The second function checks if the inverse has been computed,
## and if not computes the inverse and then sets it in the 
## makeCacheMatrix list for further use.

cacheSolve <- function(x, ...){
		I <- x$getinverse()
		if(!is.null(I)) {                      ##Check if inverse is in cache
				message("getting cached data")
				return(I)
		}
		data <- x$get()
		I <- solve(data, ...)                 ##If not compute and return inverse
		x$setinverse(I)
		I
}


