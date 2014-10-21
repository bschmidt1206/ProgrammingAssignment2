# Matrix inversion is usually a costly computation and there may be
# some benefit to caching the inverse of a matrix rather than
# computing it repeatedly. This file defines a pair of functions
# that cache the inverse of a matrix.  Key Assumption: These
# functions assume that the matrix supplied is always invertible.

# Create a special "matrix" that can cache its inverse.
# Returns a list containing four functions for:
#   getting/setting the matrix
#   getting/setting the matrix inverse
makeCacheMatrix <- function(x = matrix())
{
  # The inverse of the matrix 'x'
  inv = NULL
  
  get <- function() x
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  getinv <- function() inv
  setinv <- function(xInv) inv <<- xInv

  list(get=get, set=set, getinv=getinv, setinv=setinv)
}

# Compute the inverse of a "matrix" returned by makeCacheMatrix.
# Return the cached value if it has previously been computed.
cacheSolve <- function(x, ...)
{
  # Check for a cached value
  inv = x$getinv()

  if (is.null(inv))
  {
    message("calling solve...")
    
    # Compute inverse and cache the value
    inv <- solve(x$get(), ...)
    x$setinv(inv)
  }
  
  return (inv)
}

# testing ####

m = matrix(c(1, 2, 13, 14, 5, 6, 7, 8, 19), nrow = 3, ncol = 3)
mc = makeCacheMatrix(m)
m.inv = cacheSolve(mc)

mc2 = makeCacheMatrix(m.inv)
m2 = cacheSolve(mc2)

m.inv2 = cacheSolve(mc)
mc3 = makeCacheMatrix(m.inv2)
m3 = cacheSolve(mc3)

# Cannot use identical() to test for equality due to numeric
# approximations resulting from computation of inverse
print(m)
print(m2)
print(m3)

