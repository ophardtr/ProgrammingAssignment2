## makeCacheMatrix and cacheSolve work together to return the inverse of a matrix
## if the inverse has already been calculated, value is returned from the cache

## makeCacheMatrix produces a matrix 'x'; caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                            ## set the value of the matrix, x
                x <<- y 
                m <<- NULL
        }
        get <- function() {x}                           ## returns a matrix, x
        setsolve <- function(solve) {                   ## sets the value of the inverse of x
                m <<- solve
                                     }
        getsolve <- function () {m}                     ## returns the value of the inverse of x
        list (set = set,                                ## list of functions returned by makeCacheMatrix
              get = get, 
              setsolve = setsolve,                      
              getsolve = getsolve)
}

## cacheSolve checks if the inverse of the matrix, x, has been cached
## if not, it calculates the inverse

cacheSolve <- function(x, ...) {
        m <- x$getsolve()                               ## checks for cached value
        if(!is.null(m)){                                ## gets cached value
                message("getting cached data")
                return(m)                               ## breaks out of function
        }
        data <- x$get()                                 ## sets matrix
        m <- solve(data, ...)                           ## solves inverse of matrix                       
        m                                               ## cacheSolve returns inverse of matrix
}
