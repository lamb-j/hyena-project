# Treat every matriline as an individual and calculate their David's Score to others

# Run the header file
source("~/Documents/NIMBioS/Project/CODE/header.R")
# Load matrilines
load("matrilines.RData")
# Create a matrix of individual Clutton-Brock rank values
cluttonbrock <- matrix(0,N,length(Y))
rownames(cluttonbrock) <- tblRFA[,1]
colnames(cluttonbrock) <- Y
# Create a matrix to record natural number matriline ranks
rank <- matrix(0,M,length(Y))
rownames(rank) <- LETTERS[1:M]
colnames(rank) <- Y
# For every year y
for (y in Y)
{
  # Define z for assigning values by year
  z <- y - Y[1] + 1
  # Create a square matrix of all individuals to record agonistic interaction
  domiMatrix <- matrix(0, N, N)
  colnames(domiMatrix) <- tblRFA[,1]
  rownames(domiMatrix) <- tblRFA[,1]
  # For every agonistic interaction a
  for (a in 1:nrow(tblAggression))
  {
    # If a is dated
    if (tblAggression[a,1] != "")
    {
      # If agonistic interaction happens in year y
      if (y == as.numeric(format(as.Date(tblAggression[a,1],"%m/%d/%y"),"%Y")))
      {
        # For every individual i
        for (i in 1:N)
        {
          # If i is the aggressor
          if (tolower(tblAggression[a,2]) == tblRFA[i,1])
          {
            # If i has an adult date
            if (tblRFA[i,8] != "")
            {
              # If i is an adult at the time of interaction
              if (isAdult(i,tblAggression[a,1]))
              {
                # For every individual j
                for (j in 1:N)
                {
                  # If j is the recipient
                  if (tolower(tblAggression[a,3]) == tblRFA[j,1])
                  {
                    # If j has an adult date
                    if (tblRFA[j,8] != "")
                    {
                      # If j is an adult at the time of interaction
                      if (isAdult(j,tblAggression[a,1]))
                      {
                        # If i and j are in different matrilines
                        if (matrilix[i,j] != 1)
                        {
                          # If there is no interaction counted yet
                          if (domiMatrix[i,j] == 0)
                          {
                            # Record the presence of an agonistic interaction
                            domiMatrix[i,j] <- 1
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  # For every individual i
  for (i in 1:N)
  {
    # Define a vector of hyenas defeated by i
    B <- which(domiMatrix[i,] == 1)
    # Define a vector of hyenas that defeated i
    L <- which(domiMatrix[,i] == 1)
    # Set a placeholder for number of hyenas defeated by hyenas defeated by i
    c <- 0
    # Set a placeholder for number of hyenas that defeated hyenas that defeated i
    d <- 0
    # For every individual j defeated by i
    for (j in B)
    {
      # Add the number of hyenas defeated by j
      c <- c + rowSums(domiMatrix)[j]
    }
    # For every individual j that defeated i
    for (j in L)
    {
      # Add the number of hyenas that defeated j
      d <- d + colSums(domiMatrix)[j]
    }
    # Calculate Clutton-Brock index for each
    cluttonbrock[i,z] <- (length(B) + c + 1)/(length(L) + d + 1)
    # For all individuals i
    for (i in 1:N)
    {
      # If i is never the aggressor
      if (rowSums(domiMatrix)[i] == 0)
      {
        # If i is never the recipient of aggression
        if (colSums(domiMatrix)[i] == 0)
        {
          # Set the index value to 0 (exclusive lower limit of the Clutton-Brock index)
          cluttonbrock[i,z] <- 0
        }
      }
    }
  }
  # Create a vector to hold each matriline's highest Clutton-Brock values
  cbCut <- numeric(M)
  # For all matrilines m
  for (m in 1:M)
  {
    # For all individuals i
    for (i in 1:N)
    {
      # If individual i is in matriline m
      if (matrilines[m,i] == 1)
      {
        # If the matriline's current highest Clutton-Brock value is less than i's
        if (cbCut[m] < cluttonbrock[i,z])
        {
          # Set the matriline's highest Clutton-Brock value to i's
          cbCut[m] <- cluttonbrock[i,z]
        }
      }
    }
  }
  # Sort the Clutton-Brock values
  cbSort <- sort(cbCut,decreasing=TRUE)
  # For all matrilines m
  for (m in 1:M)
  {
    # If m is ranked
    if (cbCut[m] != 0)
    {
      # For all matrilines n
      for (n in M:1)
      {
        # If m's Clutton-Brock value is sorted to index n
        if (cbCut[m] == cbSort[n])
        {
          # The natural number rank is equal to n
          rank[m,z] <- n
        }
      }
    }
  }
}
# Save the matriline ranks
save(rank,cluttonbrock,file="rank.RData")