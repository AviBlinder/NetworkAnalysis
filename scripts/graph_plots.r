g_graph <- g
A <- get.adjacency(g_graph, sparse=FALSE) 
g <- network::as.network.matrix(A) 
library(sna)
sna::gplot.target(g, degree(g), main="Degree", 
                  circ.lab = FALSE, circ.col="skyblue", 
                  usearrows = FALSE, 
                  vertex.col=c("blue", rep("red", 32), "yellow"),
                  edge.col="darkgray") 



###########################################
v.size <- 3.5*sqrt(V(g)$FriendsNumber)

plot(g,layout=layout.fruchterman.reingold,
     vertex.size = 10,
     vertex.label=NA,
     edge.width = 0.5,
     edge.arrow.size=0.5)
###########################################
