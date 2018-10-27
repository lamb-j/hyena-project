#This script takes the table of all the observations and turns it into a graph
#in an attempt to identify the inner communities.
source("wd.R")
source("twT.R")
library('igraph')
library('cluster')

#twT is the 3-column table of the twice-weigted associations among the 170 females

dendrogram <- function(twTable, i){
  setwd(Dendrograms)

#Remove 0 and 1 entries
  twTable <- subset(twTable, Twai > 0)
  twTable <- subset(twTable, Twai < 1)

#creates an undirected graph
  twT_graph <- graph.data.frame(twTable) 
  twT_graph <- as.undirected(twT_graph, mode='collapse')

#walktrap algorithm for community detection
  twT_wt   <- walktrap.community(twT_graph, steps = 50, modularity = TRUE)

#dendrogram 
  twT_wt_dend <- as.dendrogram(twT_wt, use.modularity= TRUE)
  
#plots this dendrogram as a png
  png(filename = sprintf("twT_wt_dend_%d.png", i), width = 1200, height = 1000)
  plot(twT_wt_dend)
  dev.off()

#edgebetweeness algorithm for community detection
#  twT_eb  <- edge.betweenness.community(twT_graph)

#dendrogram
#  twT_eb_dend <- as.dendrogram(twT_eb)

#plots this dendrogram as a png
#  png(filename = sprintf("twT_eb_dend_%d.png", i), width = 1200, height = 1000)
#  plot(twT_eb_dend)
#  dev.off()
}

#loop through the yearly tables
for(i in 1:21){
  if (i == 4 || i == 17 || i == 18 || i == 20) next

  print(i)
  setwd(Tables1year)
  twTi <- read.csv(sprintf("twT%d.csv", i), stringsAsFactors = FALSE)

  dendrogram(twTi, i)
}

setwd(Code)
