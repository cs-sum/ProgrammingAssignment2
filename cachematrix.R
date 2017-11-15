## makeCacheMatrix is a list containing three functions used to cache the normal matrix
## and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	nmat <- x
	imatx <- NULL

      getn <- function() nmat
      setinv <- function(ix) imatx <<- ix
      getinv <- function() imatx
      list(	getn = getn,
		setinv = setinv,
           	getinv = getinv)
}


## cacheSolve is a function to calculate the inverse matrix from a list created using
## above makeCacheMatrix or to return the cached matrix from the list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinv()
	if (!is.null(m)) {
		message("getting inverse matrix from cache")
		return(m)
	}
	data <- x$getn()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
