rm(list=ls())

source("scripts/read_datasets.r")

source("scripts/main_loop.r")

source("scripts/gather_sna_data.r")

####################################################################
#Check: graph.neighborhood(Y_graph,order=1) for most prominent users
Y.nbhds <- graph.neighborhood(Y_graph, order=1)

ng <- (sapply(Y.nbhds, vcount))
ng[ng == max(ng)]

head(ng,500)
k.max <- which(ng == max(ng))
k.max <- 198
g.max_neig <- Y.nbhds[[414]]

plot(g.max_neig, vertex.label=NA,
     vertex.color="blue",
     vertex.size = 4)

####################################################################
##To Do
#Top 36 Pies of friends relations facetted
#
names(selected_businesses)
library(ggplot2)
ggplot(selected_businesses[1], aes(x=factor(2), fill=c(FALSES,TRUES)))+
  geom_bar(width = 1)+
  coord_polar("y")

#Analysis of final results
# --> which users are the most important
# --> there are different types of networks...(number of cliques...)
head(most_pop_user_df)
