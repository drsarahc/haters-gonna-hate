validnum <- function(num) {
    if ((num == "best") | (num == "worst")) return(TRUE)
    suppressWarnings(i <- as.integer(num))
    if (is.na(i)) return(FALSE)
    if ((i > 0) & (i <= 1000)) return(TRUE)  ## max hosp in a state = 1000
    return(FALSE)
}

rankall <- function(outcome, num="best") {
    ## Read outcome data
    ## Columns of interest are
    ##  [2] Hospital.Name
    ##  [7] State
    ## [11] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    ## [17] Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    ## [23] Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    
    ## Check that outcome and num are valid
    HA <- "heart attack"
    HF <- "heart failure"
    PN <- "pneumonia"
    Namecol <- 2
    Statecol <- 7
    HAcol <- 11
    HFcol <- 17
    PNcol <- 23
    valoutcomes <- c(HA, HF, PN)
    if (!(outcome %in% valoutcomes)) stop("invalid outcome")
    if (!(validnum(num))) stop("invalid value for num")
    
    raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ratecol = ifelse(outcome==HA,HAcol,
                ifelse(outcome==HF,HFcol,
                    ifelse(outcome==PN,PNcol,NA)))

    caremeas <- subset(raw, select=c(Namecol,Statecol,ratecol))

    ## Now caremeas has three columns:
    ## [1] Hospital name
    ## [2] State
    ## [3] The outcome rates of interest
    
    ## Make sure the rates are numeric
    caremeas[,3] <- suppressWarnings(as.numeric(caremeas[,3]))
    
    ## Ditch anything with missing rates
    caremeas <- caremeas[!is.na(caremeas[,3]),]

    ## Sort by hospital name and then by rate
    ## so that ties will be broken alphabetically
    caremeas <- caremeas[order(caremeas[,1]),]
    caremeas <- caremeas[order(caremeas[,3]),]
    
    ## Now split by state
    bystate <- split(caremeas,caremeas$State)
    
    hospital.names.list <- lapply(bystate,function(x,n) {
        index <- ifelse(n=="worst",nrow(x),
                        ifelse(n=="best",1,n))
        x[index,1:2]
    },num)
    
    hospital.names <- do.call(rbind,hospital.names.list)
    colnames(hospital.names) <- c("hospital", "state")
    ## Return a data frame with state and hospital names for req'd ranking
    return(hospital.names)

}
