setwd("~Pandora/Documents/NIMBioS/Project/DATA")

# Read in the matrilines
matrilines <- read.csv("matrilines.csv")
# Create a matrix to record internal TWAI sums for every matriline and year
internalSums <- matrix(0,nrow(matrilines),21)
rownames(internalSums) <- matrilines[,1]
colnames(internalSums) <- 1989:2009
# Create a matrix to record internal connectivity for every matriline and year
internal <- internalSums
# Remove the names column from matrilines
matrilines <- matrilines[,-1]
# Assign clan size to N
N <- nrow(matrilines)
# For every year
for (y in 1989:2009)
{
  # Define z for assigning values by year
  z <- y - 1988
  # Import a matrix of TWAI values in year y
  twai <- read.csv(sprintf("Tables/Matrices/twM_%d.csv",y))
  # For every individual i
  for (i in 1:nrow(matrilines))
  {
    # If i's matriline does not yet have an internal connectivity value for year y
    if (internal[i,z] == 0)
    {
      # If i is connected to any individual that year
      if (rowSums(twai)[i] != 1)
      {
        # Set minimum matriline size for that year to 1
        mSize <- 1
        # For every individual j
        for (j in 1:nrow(matrilines))
        {
          # Excluding TWAI values of 1 for an individual with herself
          if (i != j)
          {
            # If i and j are in the same matriline
            if (matrilines[i,j] == 1)
            {
              # If j is connected to any individual that year
              if (rowSums(twai)[j] != 1)
              {
                # Increase the size of the matriline for that year
                mSize <- mSize + 1
                # Add the i and j's TWAI value to the sum of their matriline's TWAI values
                internalSums[i,z] <- internalSums[i,z] + twai[i,j]
              }
            }
          }
        }
        # If i's matriline has multiple individuals that year
        if (mSize > 1)
        {
          # The internal connectivity of i's matriline is calculated
          internal[i,z] <- internalSums[i,z]/((mSize)*(mSize-1))
          # For every individual j
          for (j in 1:nrow(matrilines))
          {
            # If i and j are in the same matriline
            if (matrilines[i,j] == 1)
            {
              # j has the same internal connectivity
              internal[j,z] <- internal[i,z]
            }
          }
        }
      }
    }
  }
}

write.csv(internal,"internalConnect.csv")