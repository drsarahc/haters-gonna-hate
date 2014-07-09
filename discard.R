best <- function(state, outcome) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Read outcome data
    caremeas <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    get <- function() x
    setinv <- function(cacheSolve) m <<- cacheSolve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

