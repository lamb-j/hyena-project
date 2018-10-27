#This code determines which Hyenas are in which Matrilines
#It uses the motherhood data from tblRFA
#The output is a matrix with 1's if two individuals are in the same line


# Run the header file
source("wd.R")
setwd(Formatted_Data)
load("tblRFA.RData")
N <- nrow(tblRFA)

# Create a motherhood matrix
motherhood <- diag(N)
rownames(motherhood) <- tblRFA[,1]
colnames(motherhood) <- tblRFA[,1]
print("loop 1")
# For every adult female hyena i
for (i in 1:N)
{
  # For every adult female hyena j
  for (j in 1:N)
  {
  print(sprintf("i:%d, j:%d", i, j))
    # If j's mother is recorded
    if (tblRFA[j,5] != "")
    {
      # If i is j's mother
      if (as.character(tblRFA[i,1]) == as.character(tblRFA[j,5]))
      {
        # Record in the motherhood matrix
        motherhood[i,j] <- 1
      }
    }
  }
}
# Create a matrilines matrix
matrilix <- motherhood
# For every adult female hyena i
for (i in 1:N)
{
  # For every adult female hyena j
  for (j in 1:N)
  {
  print(sprintf("l2, i:%d, j:%d", i, j))
    # If i is the mother of j
    if (matrilix[i,j] == 1)
    {
      # j and i are also in the same matriline
      matrilix[j,i] <- 1
    }
  }
}

#Makes a graph from the matrix, 
#then uses equivalence relations to connect components of the corresponding graph.
#Basically a depth-first search implemented in igraph

library(igraph)
cc  <- clusters(graph.adjacency(matrilix))$membership
B   <- matrilix
B[] <- cc[row(matrilix)] == cc[col(matrilix)]

matrilix <- B

print("matrilix created")
# Create a string of matriline names
matrilineTxt <- ""
# Create a vector to count each matriline's representative index
firstIndividual <- numeric(N)
# Set M to count matrilines
M <- 0
# For every individual i
for (i in 1:N)
{
  # Create a placeholder string
  matrilineStr <- ""
  # For every individul j
  for (j in 1:N)
  {
    # If i and j are in the same matriline
    if (matrilix[i,j] == 1)
    {
      # Add j's name to i's matriline names string
      matrilineStr <- paste(matrilineStr,tblRFA[j,1])
    }
  }
  # For every individual j
  for (j in N:1)
  {
    # If i and j are in the same matriline
    if (matrilix[i,j] == 1)
    {
      # If j is an earlier instance of the matriline
      if (i > j)
      {
        # Delete the placeholder string
        matrilineStr <- ""
      }
    }
  }
  # If the placeholder string exists
  if (matrilineStr != "")
  {
    # Iterate the lettering index
    M <- M + 1
    # Count the index as matriline M's representative one
    firstIndividual[M] <- i
    # Append a new line character
    matrilineStr <- paste(matrilineStr,"\n",sep="")
    # Append the string to a letter
    matrilineStr <- paste(LETTERS[M],matrilineStr,sep="")
    # Append the string to the full one
    matrilineTxt <- paste(matrilineTxt,matrilineStr,sep="")
  }
}
# Create a text file of matriline names and their members
write(matrilineTxt,"matrilines.txt")
# Create a matrix for matriline assignment
matrilines <- matrix(0,M,N)
rownames(matrilines) <- LETTERS[1:M]
colnames(matrilines) <- tblRFA[,1]
# For every matriline m
for (m in 1:M)
{
  matrilines[m,] <- matrilix[firstIndividual[m],]
}
# Save the matriline matrices
save(matrilines,matrilix,M,file = "matrilines.RData")
setwd(Code)
