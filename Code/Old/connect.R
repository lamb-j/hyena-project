setwd("~Dropbox/NIMBioS_Research/Tables")

matrilines <- read.csv("matrilines.csv")
matrilines <- matrilines[,-1]
twMStr <- character(21)
for (y in 1989:2009)
{
  z <- y - 1988
  twMStr[z] <- sprintf("/Matrices/twM_%d",y)
}

internalSums <- matrix(0,nrow(matrilines),21)
rownames(internalSums) <- matrilines[,1]
colnames(internalSums) <- 1989:2009
internal <- internalSums

for (y in 1989:2009)
{
  twai <- read.csv(twMStr[y])
  for (i in 1:nrow(matrilines))
  {
    mSize <- 0
    for (j in 1:nrow(matrilines))
    {
      if (matrilines[i,j] == 1)
      {
        mSize <- mSize + 1
        internalSums[i,z] <- internalSums[i,z] + twai[i,j]
      }
    }
    internal[i,z] <- internalSums[i,z]/((mSize)*(mSize - 1))
  }
}

write.csv(internal,"internalConnect.csv")