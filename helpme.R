## setwd("C:/Documents and Settings/scarroll/Desktop/R")
setwd("~/Desktop/Data Science Courses/Rcode")
infile <- read.csv("gradefile.csv",header=TRUE,stringsAsFactors=FALSE)
ID <- sort(unique(infile$ID))
dummyrec <- read.csv("requiredcourses.csv",header=TRUE,stringsAsFactors=FALSE)
q <- cbind(ID,dummyrec)
row.names(q)<-ID
k <- 1  ## initialize
IPSlist <- c("PSD7035","PSD7046","PSD7047",
             "PSD7135","PSD7146","PSD7147",
             "PSD7235","PSD7245")
numlines <- dim(infile)[1]
GoodList <- unique(substr(colnames(dummyrec),1,7))
## will become a for loop, k increments by 1
for (k in 1:numlines) {
  thisID<-as.character(infile$ID[k])
  thisCourse <- substr(infile$Course.Number[k],1,7)
  if (thisCourse %in% GoodList) {
    if (thisCourse %in% IPSlist) 
      thisCourse <- paste(thisCourse,".",infile$Session[k],sep="")   
    q[thisID,thisCourse] <- infile$Grade[k]
  } else print(paste("Ignoring",thisCourse))
  ## print(thisCourse)
}
head(q)
write.csv(q,"output for FMP.csv",na="")

