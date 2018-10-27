#--------------------------------------
#Creates a table of all the pairs of Hyenas and their
#coresponding twice-weighted association index


#Read in Data
source("wd.R")
setwd(Formatted_Data)
tblRFA <-read.csv("tblRFA.csv", stringsAsFactors = FALSE)

tblS   <-read.csv("tblS.csv", stringsAsFactors = FALSE)

#For testing our program, I'm going to change the size of tblHyenas to something more manageable,
#tblS is specifically formatted to work with this shorter version
tblRFA <- head(tblRFA, 15)
#tblS   <- head(tblS, 20000)

#Create a vector and matrix of the Hyenas' IDs
#------------------

# Creates a Matrix of observations from tblSessions
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

  adult_date   <- as.Date(tblRFA[x, 8], "%Y-%m-%d")
  current_date <- as.Date(date, "%m/%d/%Y")
  
#  if(current_date < adult_date) return(1)
  return (0)
}
  
#turn the ID into a vec of ID's
ID_to_Vec <- function(i){
   x <- tblS[i,2]
   x <- unlist(strsplit(x, split=" "))
   return (x)
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
  print(i)
  date     <- tblS[i,1]
  x        <- ID_to_Vec(i)
  rfMatrix <- make_pairs(x, date, rfMatrix)
}
  
#----------------------------
# Creates a matrix of Twice-weighted Associations from the observation rfMatrix

rfNumMatrix <- data.matrix(rfMatrix)
twaiMatrix <- diag(ncol(rfNumMatrix))
# create an identity matrix of equal size as a template
for (m in 1:nrow(rfNumMatrix))
{
  for (n in 1:ncol(rfNumMatrix))
  {
    # iterate through each element in the matrix (ncol = nrow, but differentiated for clarity)
    if (m != n)
      # keep the identity values on the diagonal
    {
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
}

#-------------------------------
# Generates twTable from twMatrix

twMatrix <- twaiMatrix

twCols   <- c("Hy1", "Hy2", "Twai")
twRows   <- as.vector(tblRFA$ID) 

twTable  <- matrix(0, nrow = nrow(twMatrix)^2, ncol = 3)
colnames(twTable) <- twCols

V1 <- vector()
V2 <- vector()

#Puts the Hyenas into the vector
for(i in 1:length(twRows)){
  for(j in 1:length(twRows)){
    V1 <- c(V1, twRows[i])
  }
}

for(i in 1:length(twRows)){
  V2 <- c(V2, twRows)
}

#Puts the vectors of Hyenas into the first and second columns of the table
twTable[,1] <- V1
twTable[,2] <- V2

#Adds a pair to the table
add_twi  <-function(id1, id2){
  id1 <- match(id1, twRows)
  id2 <- match(id2, twRows)

  twi <- twMatrix[id1, id2]
  return(twi)
}

#Loops through all of the pairs in the table
l <- length(twRows)
for(k in 1:l){
  for (i in 1:l){
    for(j in i:l){
#This line is gross, but it works...
       twTable[i + (k-1)*l,3] <- add_twi(twTable[i + (k-1)*l,1], twTable[i + (k-1)*l,2])
    }
  }
}

setwd(Formatted_Data)
write.csv(twTable, "twTable.csv", row.names = FALSE)
setwd(Code) 
