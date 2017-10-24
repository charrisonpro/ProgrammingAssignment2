## THIS CODE WRITTEN TO COMPLY WITH COURSERA COURSE REQUIREMENTS FOR
## COURSE R PROGRAMMING
## WRITTEN BY CONNOR HARRISON

## This file contains R script for two functions. The first function, 
## named makeCacheMatrix, takes as argument a single matrix object.
## The matrix is assumed to be invertible. The returned object is a 'list'
## of four functions called 'set', 'get', 'setSolution', and 'getSolution'.
## Note that makeCacheMatrix does not compute the solution of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Initiate solution variable  
        s <- NULL
        
        ## Define 'set' function, which is passed an object and caches it to
        ## x in the parent frame. Clears solution varable in parent frame.
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        
        ## Define 'get' function, which just returns the matrix object.
        get <- function() x
        
        ## Define 'setSolution', which caches its argument to the parent frame.
        setSolution <- function(solution) s <<- solution
        
        ## Define 'getSolution', which returns the value of s.
        getSolution <- function() s
        
        ## RETURN statement. Assigns list names for '$' extraction operator use
        list(set = set, get = get, setSolution = setSolution,
             getSolution = getSolution)
}


## The second function, named cacheSolve, takes as argument an object stored by
## makeCacheMatrix, and other arguments to be passed to 'solve'.  cacheSolve
## calls the elements of a 'makeCacheMatrix-type' list object, and so will throw
## an error, '$ operator invalid', for any other type of object. The function
## first calls 'getSolution' and checks if it's null. If not, 'return' is called
## on the cached value. Otherwise, the function computes, caches (through 
## setsolution), and then returns the solution matrix (inverse matrix if no ...
## were used).  

cacheSolve <- function(x, ...) {

        ## Recall cached value of the solution value from the parent frame.
        s <- x$getSolution()
        
        ## IF the value is already computed, return the computed value.
        if(!is.null(s)){
                message("getting cached data")
                return(s)
                ## The function terminates here if the solution was cached.
        }
        
        ## IF getSolution returned NULL, then 'get' the matrix to solve.
        data <- x$get()
        
        ## Compute the solution.
        s <- solve(data, ...)
        
        ## Pass the computed value to setSolution to be cached.
        x$setSolution(s)
        
        ## RETURN the computed solution.
        s
}
