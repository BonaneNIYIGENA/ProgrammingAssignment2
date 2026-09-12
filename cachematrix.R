# -----------------------------------------------------------------------------
# Function: makeCacheMatrix
# Purpose:
#   Creates a special matrix object capable of storing a matrix and caching
#   its inverse. The cached inverse is cleared whenever the matrix is updated.
#
# Arguments:
#   x - A matrix to be stored. Defaults to an empty matrix.
#
# Returns:
#   A list containing accessor and mutator functions for the matrix and
#   its cached inverse.
# -----------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

    # Initialize the cached inverse as NULL because it has not been computed.
    m <- NULL

    # Updates the stored matrix and invalidates the existing cached inverse.
    # The <<- operator modifies variables in the enclosing environment.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Returns the currently stored matrix.
    get <- function() {
        x
    }

    # Stores the computed inverse in the cache.
    setsolve <- function(solve) {
        m <<- solve
    }

    # Returns the cached inverse, if one has already been computed.
    getsolve <- function() {
        m
    }

    # Return the matrix object together with its associated accessor
    # and mutator functions.
    list(
        set = set,
        get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
}


# -----------------------------------------------------------------------------
# Function: cacheSolve
# Purpose:
#   Computes the inverse of a special matrix object created by
#   makeCacheMatrix(). If the inverse has already been calculated and the
#   matrix has not changed, the cached result is returned instead of
#   performing the computation again.
#
# Arguments:
#   x   - A special matrix object created by makeCacheMatrix().
#   ... - Additional arguments passed to the solve() function.
#
# Returns:
#   The inverse of the matrix stored in x.
# -----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {

    # Retrieve the previously cached inverse, if available.
    m <- x$getsolve()

    # If an inverse has already been calculated, return the cached result
    # to avoid performing the computationally expensive operation again.
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    # Retrieve the current matrix from the special matrix object.
    data <- x$get()

    # Calculate the inverse because no valid cached result is available.
    m <- solve(data, ...)

    # Store the newly calculated inverse for future calls.
    x$setsolve(m)

    # Return the calculated inverse.
    m
}
