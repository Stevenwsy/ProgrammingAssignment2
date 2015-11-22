
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#             If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#             should retrieve the inverse from the cache.


makeCacheMatrix <- function( matrix = numeric() ) {
        
        inverse <- NULL
        
        # Set function: Set the newMatrix as matrix
        set <- function( newMatrix ) {
                matrix <<- newMatrix
                inverse <<- NULL
        }
        
        # get function: get the matrix
        get <- function() matrix
        
        # SetInvese function:   Set the invMatrix as inverse
        setInverse <- function( invMatrix ) inverse <<- invMatrix
        
        # getInverse function: get the inverse matrix
        getInverse <- function() inverse
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function( matrix, ... ) {
        
        inverse <- matrix$getInverse()
        
        # When the inverse matrix has been computed, return it
        if( !is.null(inverse) ) {
                message("getting cached data")
                return( inverse )
        }
        
        # When the inverse matrix hasn't been computed, call the get function on object matrix and 
        # compute the inverse using solve function
        data <- matrix$get()
        
        inverse <- solve( data, ... )
        matrix$setInverse(inverse)
        inverse
}


