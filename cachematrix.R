## This program includes 2 functions which can cache the inverse 
## of a matrix so that there is no need to compute the inverse
## of a matrix repeatedly.

## The "makeCacheMatrix" function can set/get a matrix and 
## set/get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y){
                         x <<- y
                         m <<-NULL
              }
              get <- function() x 
              setInv <- function(Inv) m <<- Inv
              getInv <- function() m
              list(set = set, get = get,
                   setInv = setInv,
                   getInv = getInv)
  }


## The "cachesolve" function computes the inverse of the
## matrix returned by "makeCacheMatrix" function.  If 
##the inverse has already been calculated, then cacheSolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if (!is.null(m)){
                 message("getting cached inverse of matrix")
                 return(m)
         }
        Inv <- x$get()
        m <- solve(Inv)
        x$setInv(m)
        m 
        ## Return a matrix that is the inverse of 'x'
}
