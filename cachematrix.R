## Course: R Programming. Coursera course id rprog-032
## Programming Assignment 2
##
## These 2 functions are used to calculate the inverse of a matrix and store it
## in the cache. If the cache already has the inverse of the matrix, then it 
## just returns it, and not calculate it.
##
## Version: Initial version. Created 2015 Sep 24


## _____________________________________________________________________________
## This function is called to create a list, which is implemented like an object
## It's elements include the input matrix as well as 4 functions. It takes a 
## matrix as an argument,

makeCacheMatrix <- function(InputMatrix = matrix()) {
    
    ## This code is executed to create a new list, so it must initialize the inverse
    ## value to NULL    
    InverseMatrix <- NULL
    
    ## When called, this function will set the value of the matrix to what has been
    ## passed in the inp variable. Note that the InputMatri and InverseMatrix are 
    ## defined in the parent scope and hence a <<- operator is used
    Set <- function(inp) {
        InputMatrix <<- inp
        InverseMatrix <<- NULL
    }
    
    ## When called, this function simply returns the matrix
    Get <- function() {
        InputMatrix
    }
    
    ## When called, it sets the value of the InverseMatrix with what has been passed
    ## in the inverse variable. This is used to set the value of the cache
    SetInverse <- function(inverse) {
        InverseMatrix <<- inverse
    }
    
    ## When called, it returns the value of the InverseMatrix. THis is used to fetch
    ## the value from the cache.
    GetInverse <- function() {
        InverseMatrix
    }
    
    list(Set = Set, Get = Get,
         SetInverse = SetInverse,
         GetInverse = GetInverse)
}


## _____________________________________________________________________________
## This function is used to invert the input matrix. It takes a list created
## using makeCacheMatrix as an argument. It also takes additional inputs that
## will be passed as is to the solve function that calculates the inverse of the
## matrix. Other than the input argument type, the difference between the solve
## function and this is that this function first tries to check if the inverse
## of the matrix has been stored in the cache. If yes, it does not calculate the
## inverse and instead return the cached value. If it has not been cached, it
## calculates the inverse, stores it in the cache, and then returns it.

cacheSolve <- function(InputMatrix, ...) {
    ## Get the the cached value of the inverse of the matrix
    inv <- InputMatrix$GetInverse()
    
    
    if(!is.null(inv)) {
        
        ## If the cached value is not null, then it means it has been calculated before.
        ## Show a message that the cached value is being returned.
        message("Cached value found. Returning the cache")
        return(inv)
        
    } else {
        
        ## If the cached value is null, then it has not been calculated before. Use the
        ## solve function to calculate the inverse of the matrix, store it in the cache,
        ## and then return that.
        message("Cached value not found. Calculating the inverse.")
        inv <- solve(InputMatrix$Get(), ...)
        InputMatrix$SetInverse(inv)
        return(inv)
    }
}
