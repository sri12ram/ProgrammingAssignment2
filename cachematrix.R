## Code to Caching the Inverse of a Matrix
## This code has 2 functions
##      One to create a special object that stores the matix
##      Another to Cache its Inverse

## Function to create a special List to Set and Get the values of the Matrix 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        
        ## Special List created
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Function to calculate the Inverse of a Matix from its Cache (if it exists)
cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
       
        ## Check for and Returns Cached Data (if it exists)
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}

