## Helper methods to assist in caching matrix solutions to avoid recalcution during repeat calls

## Creates a container object for a matrix, providing methods to get and set both the matrix and its solution.

makeCacheMatrix <- function(mtrx = matrix()) {

    solution <- NULL
    
    set <- function(mtrx) {
        mtrx <<- mtrx
        solution <<- NULL
    }
    
    get <- function() mtrx
    
    getsolution <- function() {
        solution
    }
    
    setsolution <- function(solution) {
        solution <<- solution
    }
    
    list(set = set, get = get, 
         getsolution = getsolution, 
         setsolution = setsolution)
}

## Returns the solution for a matrix created via makeCacheMatrix, caching that solution inside the matrix object and .....
##      returning the cached solution if available.

cacheSolve <- function(mtrx, ...) {
    
    solution <- mtrx$getsolution()
    
    if(!is.null(solution)) {
        message("returning cached data")
        return(solution)
    }
    
    solution <- solve(mtrx$get(), ...)
    mtrx$setsolution(solution)
    solution
}
