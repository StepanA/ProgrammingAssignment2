# Caching the Inverse of a Matrix

## makeCacheMatrix - function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## cacheSolve - computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse 
## has already been calculated (and the matrix has not changed), retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     matr <- x$get()
     inv <- solve(matr, ...)
     x$setinv(inv)
     inv
}

#Example
matr <- matrix(1:4,2,2)
matrcache <- makeCacheMatrix(matr)
matrinv <- cacheSolve(matrcache)

matr
matrinv
matr %*% matrinv


