source("wd.R")
#-----------------------
#Generates a twice-weight Matrix for each year, twM_year.RData


setwd(Raw_Data)
tblS <- read.csv("tblSessions.csv", stringsAsFactors = FALSE, check.names = FALSE)
setwd(Formatted_Data)
load("tblRFA.RData")

#deletes obvsv before 1/1/1989
tblS  <- tblS[-c(1:821),]

#Makes a vector of the week intervals
WeekVec <- seq(as.Date('1989-1-1'),to=as.Date('2010-1-1'),by='1 year')
WeekVec <- WeekVec[-1]


TwMatrix <- function(tblS, year){
  #----------------------
  #WRAPPER FUNTION for tblS.R
  #This function goes through the input tblS
  #and gets the twai for all individuals in tblRFA 
  #for the observations
  #The output is a 3 column table with hy1 hy2 Twai


  #------------------------
  #Simplyfy tblRFA to only include relevant hyenas

  #go through the observations and get a vector of IDs
  #do a which or something for the main tblRFA to get a new
  #table containing only the relevant Hyenas
  ID_to_Vec <- function(i){
     x <- tblS[i,2]
     x <- unlist(strsplit(x, split=" "))
     return (x)
  }

  HVec <- vector()
  for(i in 1:nrow(tblS)){  #Creates a vector of all Hyenas in obsv
    x    <- ID_to_Vec(i)
    HVec <- c(HVec, x)
  }
  HVec <- unique(HVec)
  wd <- getwd()
  setwd(Tables)
  save(HVec, file = sprintf("HVec_%d.RData", year))
  setwd(wd)
  #tblRFA <- subset(tblRFA, ID %in% HVec)


  #Create a vector and matrix of the Hyenas' IDs
  #------------------
  rfVec    <- as.vector(tblRFA$ID)
  rfMatrix <- matrix(0, nrow = length(rfVec), ncol = length(rfVec), dimnames=list(rfVec, rfVec))

  
  is_cub <- function(x, date){
  # test to see wether the two hyena's are adults for the observation
  # Args:
  #    x: The index of the hyena in the rfVec
  #    date: is the observation date
  #
  # Returns:
  #    1 if Hyena is a cub, 0 if Hyena is an adult
    if(tblRFA[x,8] == "") return(0)

    adult_date   <- as.Date(tblRFA[x, 8], "%Y-%m-%d")
    current_date <- as.Date(date, "%m/%d/%Y")

    if(current_date < adult_date) return(1)
    return (0)
  }

  #takes vector of ID's and turns them into pairs
  make_pairs <- function(x, date, Mx){
     l <- length(x)
     if(l == 0) return(Mx)

     for (i in 1:l){
       for (j in 1:l){
         Mx <- add_pairs(x[i], x[j], Mx, date)
       }
     }
     return(Mx)
  }

  #add pairs to matrix
  add_pairs <- function(x1, x2, Mx, date){

    x1 <- match(x1, rfVec)
    x2 <- match(x2, rfVec)

    #if one of the Hyenas is not in tblRFA
    if(is.na(x1) || is.na(x2)) return(Mx)
    #if one of the Hyenas is still a cub/young adult 
    if(is_cub(x1, date) || is_cub(x2, date)) return(Mx)

    Mx[x1, x2] <- Mx[x1, x2] + 1
    return(Mx)
  }

  #iterate through all of the observations
  for (i in 1:nrow(tblS)){
    #print(i)
    date     <- tblS[i,1]
    x        <- ID_to_Vec(i)
    rfMatrix <- make_pairs(x, date, rfMatrix)
  }
  print("rfMatrix created")
  #----------------------------
  # Creates a matrix of Twice-weighted Associations from the observation rfMatrix

  #print(rfMatrix)
  rfNumMatrix <- data.matrix(rfMatrix)
  twaiMatrix <- matrix(0,nrow(rfNumMatrix),ncol(rfNumMatrix))
  # create an identity matrix of equal size as a template
  if (nrow(rfNumMatrix) == 0) {
    print("NF")
    return(0)
  }

  for (m in 1:nrow(rfNumMatrix))
  {
    for (n in 1:ncol(rfNumMatrix))
    {
    # iterate through each element in the matrix (ncol = nrow, but differentiated for clarity)
      mnObs <- rfNumMatrix[m,m] + rfNumMatrix[n,n] - rfNumMatrix[m,n]
      # total number of observations of individuals m and n
      if (mnObs != 0)
        # dividing bny zero has been known to summon intergalactic demons
      {
        twaiMatrix[m,n] <- rfNumMatrix[m,n]/mnObs
        # value set to the number of pairwise observations, divided by total
      }
    }
  }

  print("twaiMatrix created")

  #-------------------------------
  rownames(twaiMatrix) <- rfVec
  colnames(twaiMatrix) <- rfVec
  print(twaiMatrix)
  return(twaiMatrix)
  #END WRAPPER FUNCTION
}


setwd(Tables)
#Loop to generate the tables for each 2-week interval
for(i in 1:length(WeekVec)){                   #cycles through the 2week intervals

  j <- 1
 
  #Computes j, the number of observations before the next date
  while(as.Date(tblS[j, 1], "%m/%d/%Y") < WeekVec[i]) j <- j + 1
  
  tblStmp <- head(tblS, j)                     #isolates the 2week interval    
  twM     <- TwMatrix(tblStmp, i + 1988) 	               #calculates table for tblStmp
  print(sprintf("date:%s i:%d", WeekVec[i], i))
  save(twM, file = sprintf("twM_%d.RData", i + 1988))    #saves table to folder
  tblS    <- tail(tblS, nrow(tblS) - j)        #removes interval from table
}

setwd(Code)
