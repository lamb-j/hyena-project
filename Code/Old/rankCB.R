#Calculate the rank of the matrilnes for each given year.
source("wd.R")
setwd(Formatted_Data)
load("tblRFA.RData")     #Hyena Table
load("tblA.RData")       #Aggression Table
load("isAdult.RData")    #isAdult function
load("matrilines.RData") #matrilines, matrilix, M

rfVec <- as.vector(tblRFA$ID)
N     <- length(rfVec)
Y     <- 1989:2009

#----------------
#Set up Containers
# Create a square matrix of all individuals to record agonistic interaction
domiMatrix <- matrix(0, N, N)
colnames(domiMatrix) <- rfVec
rownames(domiMatrix) <- rfVec

# Create a vector of individual Clutton-Brock rank values
cluttonbrock <- numeric(length(rfVec))

#Create a vector to hold each matriline's highest Clutton-Brock values
cbCut        <- numeric(M)

#Create a vector to hold the rank for each matriline 
rank_vec     <- numeric(M)

#matriline rank matrix
rank           <- matrix(0, nrow = M, ncol = length(Y))
rownames(rank) <- LETTERS[1:M]
colnames(rank) <- Y

#----------------------
#Function to calculate matriline ranks for given observations
#make a matrix of individuals
#go through the observations
#put a 1 in the row/col for aggr/recip #if both are adults of different matrilines
RankVec <- function(tblA){

  #Fills in domiMatrix
  for(i in 1:nrow(tblA)){
    agr <- match( tolower(tblA[i,2]), rfVec)
    rec <- match( tolower(tblA[i,3]), rfVec)
    
    #if one of the hyenas is not in tblRFA
    if(is.na(agr) || is.na(rec)) next
  
    #if one of the hyenas is not an adult
    if(!isAdult(agr, tblA[i,1]) || !isAdult(rec, tblA[i,1])) next
   
    #if the hyenas are in the same matrilne
    if(matrilix[agr, rec] == 1) next

    domiMatrix[agr, rec] <- 1
  }
  
  #Fills in cluttonbrock vector for individuals
  for(i in 1:N){

    # Define a vector of hyenas defeated by i and that i defeated
    B <- which(domiMatrix[i,] == 1)
    L <- which(domiMatrix[,i] == 1)
    
    c <- 0
    d <- 0
    
    # For every individual j defeated by i, add number defeated by j
    for (j in B) c <- c + rowSums(domiMatrix)[j]
    
    # For every individual j that defeated , add number that defeated j
    for (j in L) d <- d + colSums(domiMatrix)[j]
    
    # Calculate Clutton-Brock index for each individual
    cluttonbrock[i] <- (length(B) + c + 1)/(length(L) + d + 1)
     
    if(length(B) + length(L) == 0) cluttonbrock[i] <- 0
  }

  #Fills in matrlilie vector for higest ranked individuals cbCut
  for(i in 1:N){
    #If that hyena's value is higher than the value for her matriline, set it as new value
    m_index <- which(matrilines[,i] == 1)
    
    if(cluttonbrock[i] > cbCut[m_index]) cbCut[m_index] <- cluttonbrock[i]
  }
  
  #Sort the CB values
  cbSort <- sort(cbCut, decreasing = TRUE)
 
  #Get interger ranks for the matrilines
  for (m in 1:M){
    # If m is ranked
    if (cbCut[m] != 0){
      # For all matrilines n
      for (n in M:1){
        # If m's Clutton-Brock value is sorted to index n
        if (cbCut[m] == cbSort[n]) rank_vec[m] <- n
        
      }
    } 
  }
  
  return(rank_vec)
}

#Feed the RankVec the years, take the returning vector and make a matrix for all years
YVec <- seq(as.Date('1990-1-1'),to=as.Date('2010-1-1'),by='1 year')

for(i in 1:length(YVec)){
  j <- 1

  while( (as.Date(tblA[j, 1], "%m/%d/%y") < YVec[i]) && (j <= nrow(tblA)) ) {
    j <- j + 1
  }

  tblA_tmp <- head(tblA, j)
  rank[, i] <- RankVec(tblA_tmp)

  print(sprintf("date:%s i:%d", YVec[i], i))
  print(rank)
  tblA     <- tail(tblA, nrow(tblA) - j)
}

# Save the matriline ranks
setwd(Formatted_Data)
save(rank , file = "rank.RData")
setwd(Code)
