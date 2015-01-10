##
## There are two functions in this program makeCacheMatrix and cacheSolve
## respectively. Together this functions store/cache a matrix and its 
## inverse (which is lazily evaluated) thus avoiding recalculating the
## inverse everytime this call is made.
## If the input matrix is constant (data does not change) and is called
## multiple number of times during the lifetime of a program, this cache
## will save time that would have been incurred for performing the inverse
## calculation. We use R's 'solve' function to calculate the inverse of hte
## matrix.
##
####################################################################
## INSTRUCTIONS TO RUN THIS PROGRAM:
## > source ("cachematrix.R")
## > i <- matrix( c(0,2,1,0), nrow=2, ncol=2)
## > j <- makeCacheMatrix(i)
## > cacheSolve(j)
##     [,1] [,2]
## [1,]    0  0.5
## [2,]    1  0.0
## > cacheSolve(j)
## getting cached data ---> THIS OUTPUT INDICATES THAT CACHE WAS USED the 2nd TIME.
##     [,1] [,2]
## [1,]    0  0.5
## [2,]    1  0.0
####################################################################

##
## Write a short comment describing this function
#
## makeCacheMatrix - Broadly, this function represents the 'cache' storage.
## It stores two things - the input matrix x and its inverse, inv.
#
## This function is a wrapper that accepts a matrix 
## data structure (x) as an input. It performs the matrix inverse call upon 
## request (by the caller, cacheSolve in our program) and stores the inverse
## of the matrix x in inv.
#
## This function is made up of a list of functions (sub-functions, perhaps)
## called get, set, getsolve and setsolve. get and set methods are getter and setter 
## for the input matrix (x).
#
## getsolve returns the matrix 'inv' representing the inverse of 'x' if it has 
## been set earlier (using setsolve), else returns NULL to which it is initialized
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## 
## This function uses a cache (created and stored in makeCacheMatrix)
## to return the inverse of the matrix, stored in the input function x.
## 
## This function first attempts to get the cached inverse matrix value by
## calling x$getsolve. If it exists,that is, not null, it returns the value.
## If the returned value is null, the data (the original matrix) is retrieved,
## inverse calculated and the stashed away in the cache. A subsequent call
## to the function, would of course find it and use it.
#
## The return value is a matrix that is the inverse of x. If this function was
## called befoe, it resolves 
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
