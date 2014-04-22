## Stores a matrix's inverse so that it can be recalled
## without needing to recalculate it and provides a 
## function for simultaneously editing the inverse and
## storing again in this fashion



## Creates a list of functions that can serve as a
## representation of the matrix and its inverse

makeCacheMatrix <- function(A = matrix())
	{
	inv <- NULL
	setmat <- function(B)
		{
		A <<- B
		inv <<- NULL
		}
	mat <- function() {A}
	setinv <- function(Ainv) {inv <<- Ainv}
	getinv <- function() {inv}
	list(setmat = setmat, mat = mat, setinv = setinv, getinv = getinv)	
	}



## Checks in a cache to see if the matrix inverse is
## already calculated, returns the stored inverse if so
## or calculates and stores the inverse if not


cacheSolve <- function(ACache, ...)
        ## Return a matrix that is the inverse of 'x'
	{
	inv <- ACache$getinv()
	if(!is.null(inv))
		{
		message("returning cached inverse")
		return(inv)
		}
	A <- ACache$mat()
	inv <- solve(A, ...)
	ACache$setinv(inv)
	inv
	}

