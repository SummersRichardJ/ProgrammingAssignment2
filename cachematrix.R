## These functions allow the inverse of a matrix to be solved and cached for re-use to avoid 
## repeating the inverse calculation unless the matrix has changed.

## The function makeCacheMatrix creates a list of functions required to cache the inverse of an input matrix
## in the global environment.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function () x
		setinverse <- function(inv) m <<- inv
		getinverse <- function() m
		list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)

}


## The function cacheSolve displays the cached inverse of an input matrix. If no cached solution exists cacheSolve calculates the
## the inverse of the input matrix and reutrns it to makeCacheMatrix to be cached.

cacheSolve <- function(x) {
		m <- x$getinverse()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		input_matrix <- as.matrix(x$get())
		m <- solve(input_matrix)
		x$setinverse(m)
		m
}
