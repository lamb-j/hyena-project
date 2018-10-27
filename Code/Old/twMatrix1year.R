source("wd.R")
source("twM.R")

setwd(Raw_Data)
tblS <- read.csv("tblSessions.csv", stringsAsFactors = FALSE, check.names = FALSE)
setwd(Formatted_Data)
tblRFA <- read.csv("tblRFA.csv", stringsAsFactors = FALSE, check.names = FALSE)

#deletes obsvs before 1/1/1992
#tblS <- tblS[-c(1:6200),] 

#deletes obvsv before 1/1/1989
tblS  <- tblS[-c(1:821),]

#Makes a vector of the week intervals
WeekVec <- seq(as.Date('1989-1-1'),to=as.Date('2010-1-1'),by='1 year')
WeekVec <- WeekVec[-1]

setwd(Tables1year)
#Loop to generate the tables for each 2-week interval
for(i in 1:length(WeekVec)){                   #cycles through the 2week intervals

  j <- 1
 
  #Computes j, the number of observations before the next date
  while(as.Date(tblS[j, 1], "%m/%d/%Y") < WeekVec[i]) j <- j + 1
  
  tblStmp <- head(tblS, j)                     #isolates the 2week interval    
  twTbl   <- twM(tblStmp, i+1988) 	               #calculates table for tblStmp
  print(sprintf("date:%s i:%d", WeekVec[i], i))
  write.csv(twTbl, sprintf("twM_%d.csv", i + 1988), row.names=FALSE )    #saves table to folder
  tblS    <- tail(tblS, nrow(tblS) - j)        #removes interval from table
}

setwd(Code)
