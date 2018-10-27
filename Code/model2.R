rm(list =ls())
source("wd.R")
setwd(Formatted_Data)
load("A_92_99.RData")
load("E.RData")
load("i.RData")
load("r_92_99.RData")
load("b.RData")
load("mSize_Matrix.RData")
mSize_Matrix <- mSize_Matrix[, 4:11]

#Average A matrix based on number of years present
#For each matriline pair value, takes the line that was seen for the fewest years, and divides
#the value for that number of years. This represents the average connectivity between the 
#two lines for the years they were both present.
#This is accurate because all matrilines were present from the start until they died out.
for(x in 1:nrow(A)){
  for(y in 1:ncol(A)){
  msize <- as.numeric(min(length(which(mSize_Matrix[x,] > 0)), length(which(mSize_Matrix[y,] > 0)) ))
  A[x, y] <- A[x, y] / msize
  }
}

#Take out unused matrilines
A <- A[ -c(7, 12, 14, 16, 17, 19, 20), -c(7, 12, 14, 16, 17, 19, 20)] 
E <- E[-c(7, 12, 14, 16, 17, 19, 20), ]
i <- i[-c(7, 12, 14, 16, 17, 19, 20), ]
b <- b[-c(7, 12, 14, 16, 17, 19, 20), ]
r <- r[-c(7, 12, 14, 16, 17, 19, 20)]

#Take out unused years
E <- E[, 4:11]
i <- i[, 4:11]
b <- b[, 4:11]


ErowS <- rowSums(E, na.rm = TRUE)
irowS <- rowSums(i, na.rm = TRUE)
browS <- rowSums(b, na.rm = TRUE)

ErowM <- rowMeans(E, na.rm = TRUE)
irowM <- rowMeans(i, na.rm = TRUE)
browM <- rowMeans(b, na.rm = TRUE)

#print(AIC(lm(browS ~ irowS)))
#print(AIC(lm(browS ~ r + ErowS)))
#print(AIC(lm(browS ~ r + irowS)))
#print(AIC(lm(browS ~ r + r:ErowS)))
#print(AIC(lm(browS ~ r + r:irowS)))
#print(AIC(lm(browS ~ r + r:irowS + ErowS)))
#print(AIC(lm(browS ~ r + irowS + r:ErowS)))
#print(AIC(lm(browS ~ r + r:irowS + r:ErowS)))
#print(AIC(lm(browS ~ A%*%r)))
#print(AIC(lm(browS ~ A%*%r + irowS)))
#print(AIC(lm(browS ~ A%*%r + ErowS)))
#print(AIC(lm(browS ~ A%*%r + r + r:irowS + ErowS)))
#print(AIC(lm(browS ~ A%*%r + r + r:irowS + r:ErowS)))
#print(AIC(lm(browS ~ A%*%r + r:irowS + r:ErowS)))
#print(AIC(lm(browS ~ A%*%r + r + irowS + ErowS)))
#print(AIC(lm(browS ~ r + r:irowS + r:ErowS + irowS + ErowS)))
#print(AIC(lm(browS ~ r)))
#print(AIC(lm(browS ~ r + irowS + ErowS)))
#print(AIC(lm(browS ~ A%*%r + irowS + ErowS)))
 
if(F){
print(AIC(lm(browM ~ r)))
print(AIC(lm(browM ~ irowM)))
print(AIC(lm(browM ~ r + ErowM)))
print(AIC(lm(browM ~ r + irowM)))
print(AIC(lm(browM ~ r + irowM + ErowM)))
print(AIC(lm(browM ~ r + r:ErowM)))
print(AIC(lm(browM ~ r + r:irowM)))
print(AIC(lm(browM ~ r + r:irowM + ErowM)))
print(AIC(lm(browM ~ r + irowM + r:ErowM)))
print(AIC(lm(browM ~ r + r:irowM + r:ErowM)))

print(AIC(lm(browM ~ A%*%r)))
print(AIC(lm(browM ~ A%*%r + irowM)))
print(AIC(lm(browM ~ A%*%r + ErowM)))
print(AIC(lm(browM ~ A%*%r + r)))
print(AIC(lm(browM ~ A%*%r + r + ErowM)))
print(AIC(lm(browM ~ A%*%r + ErowM + irowM)))
print(AIC(lm(browM ~ A%*%r + r + ErowM + irowM)))
print(AIC(lm(browM ~ A%*%r + r:irowM)))
print(AIC(lm(browM ~ A%*%r + r:ErowM)))
print(AIC(lm(browM ~ A%*%r + r + r:irowM)))
print(AIC(lm(browM ~ A%*%r + r + r:ErowM)))
print(AIC(lm(browM ~ A%*%r + r:irowM + r:ErowM + irowM + ErowM)))
print(AIC(lm(browM ~ A%*%r + r:irowM)))
print(AIC(lm(browM ~ A%*%r + r + r:irowM + r:ErowM)))
print(AIC(lm(browM ~ A%*%r + r:irowM + r:ErowM)))
print(AIC(lm(browM ~ A%*%r + r + irowM + ErowM)))
print(AIC(lm(browM ~ A%*%r + r:irowM + r:ErowM + irowM + ErowM)))
print(AIC(lm(browM ~ A%*%r + irowM + ErowM)))
}
print(AIC(lm(browM ~ r))) #~14
print(AIC(lm(browM ~ A%*%r + r + irowM))) #~5
print(summary(lm(browM ~ A%*%r + r + irowM))) #~5
print(AIC(lm(browM ~ A%*%r + r:irowM + r:ErowM))) #~3
print(summary(lm(browM ~ A%*%r + r:irowM + r:ErowM))) #~3
print(AIC(lm(browM ~ A%*%r + r + r:irowM + r:ErowM))) #~0
print(summary(lm(browM ~ A%*%r + r + r:irowM + r:ErowM))) #~0
setwd(Code)
