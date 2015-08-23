## This program creates a list that contains a matrix, and creates
## a cache of a calculated inverse matrix so it does not need to be
## calculated multiple times

## Creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set_matrix <- function(y){
        		x <<- y
        		inverse <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(the_inverse) inverse <<- the_inverse
    get_inverse <- function() inverse
    list(set_inverse = set_inverse, get_inverse = get_inverse,
    set_matrix = set_matrix, get_matrix = get_matrix)
}

## Creates a function that returns inverse of x
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)){
        return(inverse)
    }
    matrix <- x$get_matrix()
    matrix_inverse <- solve(matrix)
  	x$set_inverse(matrix_inverse)
  	matrix_inverse
}