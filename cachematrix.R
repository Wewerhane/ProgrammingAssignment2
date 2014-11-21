## Put comments here that give an overall description of what your
## functions do

## Creates a matrix based on a vector received.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL             # Resets m to Null 
  set <- function(y) {  # takes an input Matrix
    x <<- y             # saves the input matrix
    m <<- NULL          # resets m to Null
  }
  get <- function() {x} # Returns the value of the original matrix
  setsolve <- function(solve) m <<- solve  # Called by cacheSolve () and will store the value
                                            # using superassignment
  getsolve <- function() m    # return the cached value to cacheSolve()
  list(set = set, get = get, # Accessed each time makeCacheMatrix() is called
       setsolve = setsolve,  # each time we make a new object.  This is a list of
       getsolve = getsolve) # internal functions so calling function knows how to call them in this function.
}

        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {  # The input x is a matrix created by makeCacheMatrix()
    m <- x$getsolve()  # access object x and gets to value
    if(!is.null(m)) {  # Checks to see if the inverse has already been calculated 
      message("getting cached data") # sends this message to console
      return(m)  # returns value m and ends function
    }
    data <- x$get() # run only if m is Null
    m <- solve(data, ...) # creates inverse of matrix
    x$setsolve(m) # stores the inverse of the matrix in x
    m # returns the inverse of the matrix to console
  }