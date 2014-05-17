## Put comments here that give an overall description of what your
## functions do


## This function implements a "cache matrix" which is actually
## a list with 4 sub functions to...
## 1. set the pre-calculated matrix
## 2. get the pre-calculated matrix
## 3. set the inverse (via solve function) of the matrix
## 4. get the inverse of the matrix
## 
makeCacheMatrix <- function(x = matrix()) {
    ## m will contain the inversed matrix
    m <- NULL
    
    ## Declare subfunction set
    ## - initialises value and set the inverse to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Declare subfunction get
    ## - return pre-calculated matrix
    get <- function() x
    
    ## Declare subfunction setsolve
    ## - assign/set inverse matrix to m
    setsolve <- function(solve) m <<- solve
    
    ## Declare subfunction getsolve
    ## - return m (inverse matrix)
    getsolve <- function() m
    
    ## Finally, create list containing function return values 
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}

## This function accepts a list or "cache matrix" created in the function above
## and calculates the inverse storing the result in cache.  If the matrix has 
## previously been calculated, then the inverse calculation will be skipped and
## the result will be retrieved from cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Firstly, attempt to retrieve from "cache" using getsolve function
    m <- x$getsolve()
    ## If we've found something (i.e. not null), then...
    ## ... return matrix with msg that cache lookup has been successful
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Otherwise, obtain the matrix from the get function...
    data <- x$get()
    ## and pass it to solve function to calculate inverse
    m <- solve(data, ...)
    ## finally, save to "cache" using setsolve function... 
    x$setsolve(m)
    ## ..and return inversed matrix
    m    
}
