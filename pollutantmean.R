pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ##  IMPORTANT:  will break if monitor numbers are more than 3 digits
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    
    # initialize local variables
    totpoll <- 0
    totN <- 0
    filename <- "FOO.csv"
    j=1
    
    # loop until all files read
    for (j in id)
    {
        # build name of next file
        filename <- paste(sprintf("%03d",j),".csv",sep="")
        
        # build filename including path
        wholename <- paste(directory,"/",filename,sep="")
        
        # read the next file
        curfile <- read.csv(wholename)
        
        # create vector “good” of non-missing values for desired pollutant
        pollutantvec <- curfile[pollutant]
        good <- pollutantvec[!is.na(pollutantvec)]
        
        # increment running total and number of pollutant values
        totpoll <- totpoll + sum(good)
        totN <- totN + NROW(good)
    }
    
    # return final value
    pollutantmean <- totpoll / totN
    
}

