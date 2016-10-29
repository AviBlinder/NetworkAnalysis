
rm(list=ls())
Sys.setlocale("LC_ALL", "C")
##The cliché goes that the world is an increasingly interconnected place, 
# and the connections between different entities are often best represented 
#with a graph. 
# Graphs are comprised of vertices (also often called "nodes") and edges 
# connecting those nodes. 

library(igraph)
edges <- read.csv("data/relation_friends.csv")
users <- read.csv("data/node_users.csv")
nrow(edges)
nrow(users)
mean_users <- nrow(edges)*2/nrow(users)
mean_users

#Creating a graph object
g1 <- graph.data.frame(edges,directed = FALSE) 
g1_mini <- graph.data.frame(edges[1:200,],directed = FALSE) 



##In our graph, the "degree" of a node is its number of friends. 
# some nodes in our graph have degree 0 (these are the nodes with no friends), 
# while others have much higher degree. We can use degree(g) to compute 
# the degree of all the nodes in our graph g.

degree <- degree(g1)/2
min(degree)
degreeDF <- data.frame(degree)
round(prop.table(table(degreeDF$degree >= 1 & degreeDF$degree <= 100)),2)
hist(degreeDF$degree[degreeDF$degree >= 1 &degreeDF$degree <= 100 ])


library(ggplot2)
df <- data.frame(degreeDF$degree[degreeDF$degree >= 1 &degreeDF$degree <= 100 ])


names(df) <- "degree"

ggplot(df,aes(x=degree))+
      geom_histogram(bins = 100,breaks= seq(0,100,by=1),
                     col="red",fill="gray",alpha = .2)+
#                     aes(fill=..count) 
      labs(title="Degree of Friends (between 1 and 100 friends") +
      labs(x="Number of Friends", y="Count")+
      theme_gray()
#       xlim(c(1,100))
    
df1 <- data.frame(degreeDF$degree[degreeDF$degree >= 1 &degreeDF$degree <= 10 ])
names(df1) <- "degree"
head(df1)
count <- as.data.frame(table(df1$degree))
count

ggplot(df1,aes(x=degree))+
  geom_bar()+
#                 col="red",fill="gray",alpha = .2)+
  labs(title="Degree of Friends (between 1 and 10 friends)") +
  labs(x="Number of Friends", y="Count")+
    theme_gray()



###
#In a network, it's often visually useful to draw attention to "important" nodes
# in the network. While this might mean different things in different contexts, 
# in a social network we might consider a user with a large number of friends 
# to be an important user. 

#To visually draw attention to these nodes, we will change the size of the 
# vertices so the vertices with high degrees are larger. 
# To do this, we will change the "size" attribute of the vertices of our
# graph to be an increasing function of their degrees:
degree(g)
#V(g)$size = degree(g)/2+2
V(g1)$size = degree(g1)
g1[V(g1)$size <= 2]
plot(g, vertex.label=NA)

degree <- degree(g1)
degreeDF <- data.frame(degree)
sum(degreeDF$degree >= 10)
max(degreeDF$degree/2 + 2)
min(degreeDF$degree/2 + 2)
range(degreeDF$degree)

##We can update the colors by setting the color to black for all vertices, 
# then setting it to red for the vertices with gender A and setting it to gray for the vertices with gender B:
table(V(g)$size)
V(g)$color = "black"
V(g)$color[V(g)$size > 22] = "red"
table(V(g)$color)
plot(g, vertex.label=NA)
#install.packages("rgl")
rglplot(g, vertex.label=NA)

####
#https://github.com/kolaczyk/sand/blob/master/sand/inst/code/chapter2.R
is.connected(g1, mode="weak")
is.directed(g1)
#diameter(g1, weights=NA)
vcount(g1)  ##vertex (nodes) count
ecount(g1)  ##edges (relations) count
is.simple(g1)
?is.simple
neighbors(g1,4)
degree(g1,4)
###
#Ploting 
head(g1)

#layout.drl (graph, use.seed = FALSE, seed = matrix(runif(vcount(graph) * 2), ncol = 2), 
#            options = igraph.drl.default, weights = E(graph)$weight, 
#            fixed = NULL, dim = 2) 
plot(layout.drl(g1))

fruchterman.reingold_plot <- plot(g1_mini,layout=layout.fruchterman.reingold)

layout.fruchterman.reingold.grid(g1_mini)

##
dd.g1 <- degree.distribution(g1)
d <- 1:max(degree(g1))-1
ind <- (dd.g1 != 0)
plot(d[ind], dd.g1[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

plot(g1_mini, layout=layout.kamada.kawai)

eb <-  edge.betweenness(g1_mini)

cl1 <- cliques(g1_mini)

