#This code calculates the fitness for each matriline
#The algoritm is based on percapita fitness.
source("wd.R")
setwd(Raw_Data)
tblHyenas <- read.csv("tblHyenas.csv", stringsAsFactors = FALSE)

setwd(Formatted_Data)
load("tblRFA.RData")
load("matrilines.RData")
load("mSize_Matrix.RData")

Y     <- 1989:2009
mSize <- mSize_Matrix
# Create a matrix of fitness values for all matrilines for all years
fitness <- matrix(0,M,length(Y))
rownames(fitness) <- LETTERS[1:M]
colnames(fitness) <- Y
# For every year
for (y in Y)
{
  # Define z for assigning values by year
  z <- y - Y[1] + 1
  # Create a vector to count births in a matriline
  births <- numeric(M)
  # For every matriline m
  for (m in 1:M)
  {
    # For every individual i in m
    for (i in which(matrilines[m,]==1))
    {
      # For every child of i
      for (j in which(as.character(tblHyenas[,13])==as.character(tblRFA[i,1])))
      {
        print(sprintf("i:%d j:%d", i, j))
        # If j has a birth date
        if (tblHyenas[j,14] != "")
        {
          # If j was born in year y
          if (y == as.numeric(format(as.Date(tblHyenas[j,14],"%m/%d/%y"),"%Y")))
          {
            # Count the birth of j towards m's births
            births[m] <- births[m] + 1
          }
        }
        # If j has no birth date but has a first seen date
        else if (tblHyenas[j,10] != "")
        {
          # If j was first seen in year y
          if (y == as.numeric(format(as.Date(tblHyenas[j,10],"%m/%d/%y"),"%Y")))
          {
            # Count the birth of j towards m's births
            births[m] <- births[m] + 1
          }
        }
      }
    }
  }
  # For every matriline m
  for (m in 1:M)
  {
    # If the matriline is present
    if (mSize[m,z] != 0)
    {
      # m's fitness in y is its per capita births relative to clan's per capita births
      fitness[m,z] <- (births[m]/mSize[m,z])/(sum(births)/sum(mSize[,z]))
    }
  }
}
# Save the matriline fitness values
save(fitness,file = "fitness.RData")
setwd(Code)
