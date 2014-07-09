corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    temp <- complete("specdata")
    
    # temp is a dataframe with 2 vars, id and nobs
    # how many of the items in temp are above threshold?
    areabovet <- temp$nobs>threshold
    
    numabovet <- sum(areabovet)
    if (numabovet == 0) {return(NULL)} else {
        
        # define a new vector for the ids of files to review
        ids2check <- temp$id[areabovet]
        
        # initialize a data frame to hold monitor ids and correlations
        corframe <- data.frame(id=1:numabovet,pearson=1:numabovet)
        
        # now look in each of the appropriate files and calculate corr between
        # sulfates and nitrates
        
        for (j in 1:numabovet)  {
            # build file name and open file
            # build name of next file
            filename <- paste(sprintf("%03d",ids2check[j]),".csv",sep="")
            
            # build filename including path
            wholename <- paste(directory,"/",filename,sep="")
            
            # read the next file
            curfile <- read.csv(wholename)
            
            # create vector of correlations btw sulfates and nitrates for this file
            corframe$id <- ids2check[j]
            corframe$pearson[j] <- cor(curfile$sulfate, curfile$nitrate, use="complete.obs", method="pearson")
        }
        
        corrs <- corframe$pearson
        return(corrs)
    }
}

