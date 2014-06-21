## These functions are meant to be used
## to compute matrix inverse

## The function cache a matrix inverse in order to prevent 
## another compuation matrix inverse

makeCacheMatrix <- function(matrx = matrix()) {

		inverse <- NULL
    set <- function(y) {
			matrx <<- y
			inverse <<- NULL
    }
		get <- function(){matrx}
		setInverse <- function(solve) inverse <<- solve
		getInverse <- function() inverse
		list(set = set, get = get,
				 setInverse = setInverse,
				 getInverse = getInverse)

}

## This funciton give a matrix inverse
## It computes the inverse of a matrix 
## if the inverse was never computes.
## It gets a cached matrix inverse if
## the inverse was already computed.


cacheSolve <- function(matrx, ...) {
  ## Return a matrix that is the inverse of 'matrix'
	inverse <- matrx$getInverse()
  if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- matrx$get()
	inverse <- solve(data, ...)
	matrx$setInverse(inverse)
	inverse
}
