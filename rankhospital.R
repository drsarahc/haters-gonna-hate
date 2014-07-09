validnum <- function(num) {
    numforstate <- 400
    if ((num == "best") | (num == "worst")) return(TRUE)
    suppressWarnings(i <- as.integer(num))
    if (is.na(i)) return(FALSE)
    if ((i > 0) & (i <= numforstate)) return(TRUE)
    return(FALSE)
}

rankhospital <- function(state, outcome, num="best") {
    ## Read outcome data
    ## Columns of interest are
    ##  [2] Hospital.Name
    ##  [9] State
    ## [11] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    ## [17] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    ## [23] Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    caremeas <- data.frame (Hstate = raw$State, Hname = raw$Hospital.Name, 
                            HAmort = suppressWarnings(as.numeric(raw[,11])),
                            HFmort = suppressWarnings(as.numeric(raw[,17])),
                            PNmort = suppressWarnings(as.numeric(raw[,23])))
    
    ## Check that state and outcome are valid
    HA <- "heart attack"
    HF <- "heart failure"
    PN <- "pneumonia"
    valoutcomes <- c(HA, HF, PN)
    valstates <- unique(caremeas$Hstate)
    if (!(state %in% valstates)) stop("invalid state")
    if (!(outcome %in% valoutcomes)) stop("invalid outcome")
    if (!(validnum(num))) return("NA")
    
    ## Split the big frame by state
    bystate <- split(caremeas, caremeas$Hstate)
    
    ## Assign the desired mortality data to ratevec
    if (outcome == HA) ratevec <- bystate[[state]]$HAmort
    if (outcome == HF) ratevec <- bystate[[state]]$HFmort
    if (outcome == PN) ratevec <- bystate[[state]]$PNmort
    
    ## Assign the desired hospital name data to hnamevec
    hnamevec <- bystate[[state]]$Hname
    
    ## Rank the values and then remove NAs
    rankvec <- rank(ratevec, na.last = TRUE, ties.method="first")
    s <- data.frame (rank = rankvec, rate = ratevec, hname = hnamevec)
    stateframe <- s[complete.cases(s),]
    totranks <- length(stateframe$rank)
    
    if (num == "best") {
        ## Find row number of hosp with lowest mortality rate
        returnrow <- which.min(stateframe$rate)
        print("best")
        }
    else {
        if (num == "worst") {
            ## Find row number of hosp with highest mortality rate
            returnrow <- which.max(stateframe$rate)
            print("worst")
            } 
            else {
                ## num must be a rank, but is it in bounds?
                if (num > totranks) { return("NA")}
                returnrow <- which(stateframe$rank == num)
            }
        }
    
    ## Return name of hospital with lowest mortality rate
    ## Note: does not return all hospital names in the event of a tie
    returnhosp <- as.character(stateframe$hname[returnrow])
    return(returnhosp)
}
