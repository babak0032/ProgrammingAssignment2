## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL              ##Setting the inverse to the NULL value
	set <- function(y) {	     ##Set the matrix
		x <<- y
		m <<- NULL
	}
	get <- function() x		##Get that matrix
      setInverse <- function(i) inverse <<- i		##Set the inverse matrix to i(user's chosen matrix)
      getInverse <- function() inverse			##Get the inverse matrix
      list(set = set, get = get,				##Return the list
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { 
        inverse <- x$getInverse()		##Getting the inverse from the list to check if it is NULL or a matrix.
        if(!is.null(inverse)) {		##If it wasn't NULL, it would just return the inverse from the list.
                message("getting cached data")
                return(inverse)
        }
        originalMatrix <- x$get()		##If it was NULL, then we store the original matrix in the variable "originalMatrix"
        inverse <- solve(originalMatrix, ...)		##Then we find its inverse(if it exists) using the "solve()" function and store it in the "inverse" variable
        x$setInverse(inverse)		##Setting the inverse matrix in he list and return it
        inverse
}