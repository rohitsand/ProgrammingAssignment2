## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## Unit test:
## m1 = rbind(c(1, -1/4), c(-1/4, 1))
## class(m1)
## [1] "matrix"
## mcm <- makeCacheMatrix(m1)
## cacheSolve(mcm)
##          [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## cacheSolve(mcm)
## getting cached data
##          [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


## This function creates a special "matrix" object that 
## can cache its inverse. It returns a list of functions that can 
## get and set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) 

{
	m <- NULL
      set <- function(y) 
	{
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)
}


## This function calculates and returns the inverse of a matrix. 
## If inverse of the matrix is available in cache it uses that 
## otherwise it calculates the inverse inverse and returns that

cacheSolve <- function(x, ...) 
{
	## Return a matrix that is the inverse of 'x'

	# try to get inverse from the cache	
	m <- x$getInverse()
	
	if(!is.null(m)) 
	{
		# Inverse found in cache
		message("getting cached data")
		return(m)
	}
	
	# Inverse not found in cache so calculate it
	data <- x$get()
	m <- solve(data, ...)
	
	# Now cache the inverse that we just calculated
	x$setInverse(m) 

	# return result (invert of the matrix)
	m
}
