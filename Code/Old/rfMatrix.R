#Read in Data
source("wd.R")
setwd(Formatted_Data)
tblRFA <-read.csv("tblRFA.csv", stringsAsFactors = FALSE)

setwd(Raw_Data)
tblS   <-read.csv("tblSessions.csv", stringsAsFactors = FALSE)

#For testing our program, I'm going to change the size of tblHyenas to something more manageable,
#tblS is specifically formatted to work with this shorter version
tblRFA <- head(tblRFA, 5)
#tblS   <- head(tblS, 10000)

#Create a vector and matrix of the Hyenas' IDs
rfVec    <- as.vector(tblRFA$ID)
rfMatrix <- matrix(0, nrow = length(rfVec), ncol = length(rfVec), dimnames=list(rfVec, rfVec))


#------------------
# MATRIX MANIPULATION

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
  
print(rfMatrix)
setwd(Formatted_Data)
write.csv(rfMatrix, "rfMatrix.csv", row.names = FALSE)
setwd(Code) 
