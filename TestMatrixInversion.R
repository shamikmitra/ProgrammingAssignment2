## This function is used to test the makeCacheMatrix and cacheSolve functions to be created as part of the
## Coursera R programming course. It creates a random square matrix, passes it to makeCacheMatrix to create a
## list and then calls the cacheSolve function twice. The first time it should inverse the matrix. The second
## time it should fetch from cache.
##
## Version history: Initial version. Created by Shamik Mitra. 2015 Sep 24

test <- function() {
    
    ## Ask the user to enter a number. This number will be the number of rows and columns of the test matrix
    suppressWarnings(Parm <- as.numeric(readline("Enter duration parameter. The higher the number, the longer it takes. Duration: ")))
    if(is.na(Parm)) {
        ## If the input is not a number, then show a message and exit
        message("You need to enter a numeric value. Exiting function.")
        rm(list=ls())        
    } else{
        ## Open the source code where the 2 functions have been defined
        source("cachematrix.R")
        
        ## Create a test matrix. Also create the inverse of the matrix to test the output later
        message("Creating a random matrix and its inverse to test the program ...")
        TestMatrix <- matrix(runif(Parm^2), nrow=Parm, ncol=Parm)
        OutputCheck <- solve(TestMatrix)
        message(paste("Done.","\n","\n"))

        ## Call the makeCacheMatrix function to create the list
        message("Calling the makeCacheMatrix function ...")
        TestObj <- makeCacheMatrix(TestMatrix)
        message(paste("Done.","\n","\n"))

        ## Call the cacheSolve function for the first time. Calculate the time it takes to execute the function
        message("Calling the cacheSolve function for the first time ...")
        starttime <- Sys.time()
        InvObj <- cacheSolve(TestObj)
        endtime <- Sys.time()
        firstduration <- endtime - starttime
        message(paste("Done.","\n","\n"))
                
        ## Call the cacheSolve function for the second time. Calculate the time it takes to execute the function
        ## It should take less time to calculate this
        message("Calling the cacheSolve function for the second time ...")
        starttime <- Sys.time()
        InvObj <- cacheSolve(TestObj)
        endtime <- Sys.time()
        secondduration <- endtime - starttime
        message(paste("Done.","\n","\n"))
                
        if(identical(InvObj, OutputCheck)){
            ## Check if the returned matrix inverse is the same as the inverse calculated above. If it is, then
            ## also display the times it took for each run. The second run should be much less than the first
            message("The function has returned a valid matrix inverse")
            message(paste("The first run took  ", sprintf("%0.3f", firstduration), " seconds"))
            message(paste("The second run took ", sprintf("%0.3f", secondduration), " seconds"))
        } else {
            message("The function did not return a valid matrix inverse")
        }
        
        ## Remove all the temporary vectors created
        rm(list=ls())        
    }
}