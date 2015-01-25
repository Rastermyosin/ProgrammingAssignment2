####################################################################
# cachematrix
#   Desc: Includes functions to create a cache of x and the inverse
#         of x, return the inverse of x, and methods to interact
#         with the cached values.
#
#   Author: Kevin O'Neill
#   Date: 2015.01.25
#
#   Usage: 
#
#   x = matrix(1:4,2,2) # Create a matrix
#
#   mat = makeCahceMatrix(x) # Save x to a special object which 
#                            # includes a cached inverse of x
#
#   invx = cacheSolve(mat) # Returns the inverse of x and stores the
#                          # inverse in the mat object as a cache
#
####################################################################

# mat = makeCaheMatrix(x)
# Makes an object with two values (x and inverse of x) and adds several
# methods to operate on those two values
makeCacheMatrix <- function(x = matrix()) {
  
  # Set inverse of x to be NULL
  m <- NULL 
  
  # Create the set() method. Sets the value of x and inverse of x
  set <- function(y) {
    x <<- y    # Sets x
    m <<- NULL # Sets inverse of x to be NULL whenever x changes
  } # END FUNCTION
  
  # Creates the get() function to return the matrix x
  get <- function() x
  
  # Creates the setInverse() function set inverse of x
  setInverse <- function(solve) m <<- solve
  
  # Creates the getInverse() function to return inverse of x
  getInverse <- function() m
  
  # Creates the list of methods available for use when operating with the
  # output of makeCacheMatrix(x)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
} # END FUNCTION
  

# invx = cacheSolve(mat)
# Returns the inverse of x from a cache if x has not changed
# IFF x has changed (i.e.: m = NULL) recalculate, set, and return inverse of x
cacheSolve <- function(mat, ...) {
  
  # Saves a copy of the inverse of x to a local variable
  m <- mat$getInverse()
  
  # Checks to see if m is NOT null. (This happens when x is first set, or when x has changed)
  if(!is.null(m)) {
    
    # Display to the user that cached data is being used
    message("getting cached data") 
    
    return(m) # Return the inverse of x
  } # END IF
  
  # Get x from the mat object
  data <- mat$get()
  
  # Solve for the inverse x
  m <- solve(data, ...)
  
  # Save the inverse of x to the mat object
  mat$setInverse(m)
  
  return(m) # Return the inverse of x
} # END FUNCTION

# EOF