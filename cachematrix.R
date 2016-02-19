## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function makecachematrix, creates a matrix object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv_mat <- NULL
      set <- function(y) {
      x <<- y
      inv_mat <<- NULL
  }
  get <- function() x
  set_inv_matrix <- function(inverse) inv_mat <<- inverse
  get_inv_matrix <- function() inv_mat
  list(set = set, get = get,
       set_inv_matrix = set_inv_matrix,
       get_inv_matrix = get_inv_matrix)
}

## Write a short comment describing this function
## The below function computes the inverse of a matrix object. However, before computing
## the inverse it checks for the cache value of the inverse and also if there
## is any change in matrix. If no change in matrix is observed it 
## retrieves the cache value rather than computing matrix inverse again 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$get_inv_matrix()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  mat <- x$get()
  inv_mat <- solve(mat, ...)
  x$set_inv_matrix(inv_mat)
  inv_mat
  }
