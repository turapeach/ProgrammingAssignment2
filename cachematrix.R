## cachematrix.R  includes two functions
## makeCacheMatrix and cacheSolve

# Creates new environment space and returns list of 4 functions
# set, get, setinv, getinv
# both matrix and matrix inverse are stored inside this environment
makeCacheMatrix <- function(x = matrix()) {

      xinv <- NULL                # initialize matrix inverse to NULL
      
      set <- function(xinput) {   # function to set data matrix values
            x <<- xinput          # set matrix value
            xinv <<- NULL         # when getting new data, re-initialize inverse
      }
      
      get <- function() x         # function to return data matrix value
      
                                  # function to set data matrix inverse
      setinverse <- function(xinvinput) xinv <<- xinvinput
      
      getinverse <- function() xinv   # function to return data matrix inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

# Takes a CacheMatrix list as input
# If matrix inverse already exists, it is returned
# If not, it is computed, set in the CacheMatrix list, and returned
cacheSolve <- function(x) {
      
      xinv <- x$getinv()      # Get CacheMatrix xinverse
      
      if(!is.null(xinv)) {    # Check if it exists, if so exits returning value
            message("getting cached data")
            return(xinv)
      }
      
      data <- x$get()            # If not, gets the CacheMatrix x matrix
      
      xinv <- solve(data)        # Calculates the matrix inverse
      
      x$setinv(xinv)             # Sets CacheMatrix xinverse value
      xinv
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
