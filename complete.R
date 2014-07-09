complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # initialize local variables
    filename <- "FOO.csv"
    j=1
    idlen <- length(id)
    result <- data.frame(id=1:idlen,nobs=1:idlen)
    
    # loop until all files read
    for (j in 1:idlen)
    {
        # build name of next file
        filename <- paste(sprintf("%03d",id[j]),".csv",sep="")
        
        # build filename including path
        wholename <- paste(directory,"/",filename,sep="")
        
        # read the next file
        curfile <- read.csv(wholename)
        
        # create vector “comp” of complete cases for this file
        comp <- complete.cases(curfile)
        
        result$id[j] <- id[j]
        result$nobs[j] <- sum(comp)
        
    }
    
    # return final value
    return(result)
    
}

