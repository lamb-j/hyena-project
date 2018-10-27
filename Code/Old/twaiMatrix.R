source("wd.R")
setwd(Formatted_Data)

rfMatrix <- read.csv("rfMatrix.csv", stringsAsFactors = FALSE, check.names = FALSE)
# read in a matrix - this might have to be changed for iterating by 2 weeks


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

write.csv(twaiMatrix, "twaiMatrix.csv", row.names = FALSE)
setwd(Code)
