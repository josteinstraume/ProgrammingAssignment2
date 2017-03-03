## Uses scoping rules of the R language to manipulate
## and preserve the state inside of an R object.

## In particular, caches potentially time-consuming
## compuations of calculating the inverse of a given matrix,
## assuming the given matrix is solvable.


## Creates a list which contains a function that does the following:
## Sets a matrix
## Gets a matrix
## Sets the inverse of a matrix
## Gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x

	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of a given matrix,
## assuming the given matrix is invertible/solvable.

## If the inverse has already been calculated,
## returns the already calculated inverse and does not re-do the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        	message("getting cached data")
		return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
