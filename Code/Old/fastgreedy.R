#This script takes the table of all the observations and turns it into a graph
#in an attempt to identify the inner communities.
source("wd.R")
source("twT.R")
library('igraph')
library('cluster')

#twT is the 3-column table of the twice-weigted associations among the 170 females

fastgreedy <- function(twTable, i){
  setwd(Fastgreedy)

#Remove 0 and 1 entries
  twTable <- subset(twTable, Twai > 0)
  twTable <- subset(twTable, Twai < 1)

#creates an undirected graph
  twT_graph <- graph.data.frame(twTable) 
  twT_graph <- as.undirected(twT_graph, mode='collapse')

#fastgreedy clustering algorithm
  fcs <- fastgreedy.community(simplify(as.undirected(twT_graph)))

#maximize modularity
  memb <- community.to.membership(twT_graph, fcs$merges, steps=which.max(fcs$modularity)-1)

#sets up the plot
  colbar <- rainbow(2)
  col <- colbar[memb$membership+1]
  l <- layout.kamada.kawai(twT_graph, niter=100)
  
#plots as a png
  png(filename = sprintf("twT_fcs_%d.png", i), width = 1200, height = 1000)
  plot(twT_graph, layout=l, vertex.size=6, vertex.color=col) #, vertex.label=NA)
  dev.off()

}

#loop through the yearly tables
for(i in 1:21){

  print(i)
  setwd(Tables1year)
  twTi <- read.csv(sprintf("twT%d.csv", i), stringsAsFactors = FALSE)

  fastgreedy(twTi, i)
}

setwd(Code)
