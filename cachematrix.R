## create matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     invr <- NULL
     set <- function(y) {
             x <<- y  # <<- assigns a 'global' variable
             invr <<- NULL
     }
     get <- function() x  # same-line functions no brackets required
     setInverse <- function(inverse) invr <<- inverse
     getInverse <- function() invr
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## computes the inverse of matrix, or retrieves it if already calculated

cacheSolve <- function(x, ...) {
     ## Return inverse of matrix 
     invr <- x$getInverse()
     if (!is.null(invr)) {
             message("retrieving cached data")
             return(invr)
     }
     matx <- x$get()
     invr <- solve(matx, ...)  # solve function default is inverse
     x$setInverse(invr)
     invr
}

