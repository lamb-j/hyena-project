#Calculate the rank of the matrilnes for each given year.
source("wd.R")
setwd(Formatted_Data)
load("tblRFA.RData")     #Hyena Table
load("tblA.RData")       #Aggression Table
load("isAdult.RData")    #isAdult function
load("matrilines.RData") #matrilines, matrilix, M

rfVec <- as.vector(tblRFA$ID)
N     <- length(rfVec)
Y     <- 1994:1999

#----------------
#Set up Containers
# Create a square matrix of all individuals to record agonistic interaction
domiMatrix <- matrix(0, M, M)
colnames(domiMatrix) <- LETTERS[1:M]
rownames(domiMatrix) <- LETTERS[1:M]

domiMatrix_All <- domiMatrix

# Create a matrix to record agonistic interaction proportions
P <- domiMatrix

#matriline rank matrix
rank           <- matrix(0, nrow = M, ncol = 1)
rownames(rank) <- LETTERS[1:M]
colnames(rank) <- "Total_DS"

rank_vec     <- numeric(M)
#----------------------
#Function to calculate matriline ranks for given observations
#make a matrix of individuals
#go through the observations
#put a 1 in the row/col for aggr/recip #if both are adults of different matrilines
RankVec <- function(tblA){

  #Fills in domiMatrix
  for(i in 1:nrow(tblA))
    {
    agr <- match( tolower(tblA[i,2]), rfVec)
    rec <- match( tolower(tblA[i,3]), rfVec)
    
    #if one of the hyenas is not in tblRFA
    if(is.na(agr) || is.na(rec)) next
  
    #if one of the hyenas is not an adult
    if(!isAdult(agr, tblA[i,1]) || !isAdult(rec, tblA[i,1])) next
   
    #if the hyenas are in the same matrilne
    if(matrilix[agr, rec] == 1) next

    # get the matriline index for each
    mAgr <- as.integer(which(matrilines[,agr]==1))
    mRec <- as.integer(which(matrilines[,rec]==1))
    
    #count the aggressive interaction
    domiMatrix[mAgr,mRec] <- domiMatrix[mAgr,mRec] + 1
  }
    domiMatrix_All <<- domiMatrix_All + domiMatrix
    
    domiMatrix <- domiMatrix_All
   
  # for every matrilineal pair
  for (i in 1:M) {
    for (j in 1:M) {
      #if i and j aggressively interact
      if (domiMatrix[i,j] + domiMatrix[j,i] != 0) {
        #record the proportion of i's wins against j
        P[i,j] <- domiMatrix[i,j]/(domiMatrix[i,j] + domiMatrix[j,i])
      }
    }
  }
  
  #for every matriline
  for (m in 1:M)
  {
    #define David's Score variables
    w <- rowSums(P)[m]
    l <- colSums(P)[m]
    w2 <- 0
    l2 <- 0
    for (n in 1:M)
    {
      w2 <- w2 + P[m,n]*rowSums(P)[n]
      l2 <- l2 + P[n,m]*colSums(P)[n]
    }
    rank_vec[m] <- w + w2 - l- l2
  }

  return(rank_vec)
}

#Feed the RankVec the years, take the returning vector and make a matrix for all years
YVec <- seq(as.Date('1995-1-1'),to=as.Date('2000-1-1'),by='1 year')

for(i in 1:length(YVec)){
  j <- 1

  while( (as.Date(tblA[j, 1], "%m/%d/%y") < YVec[i]) && (j <= nrow(tblA)) ) {
    j <- j + 1
  }

  tblA_tmp  <- head(tblA, j)
  rank[, 1] <- RankVec(tblA_tmp)

  print(sprintf("date:%s i:%d", YVec[i], i))
  print(rank)
  tblA     <- tail(tblA, nrow(tblA) - j)
}

r <- rank

# Save the matriline ranks
save(r, file = "r.RData")
setwd(Code)
