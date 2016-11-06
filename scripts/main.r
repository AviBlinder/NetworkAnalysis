Sys.setlocale("LC_ALL","C")
rm(list=ls())

source("scripts/read_datasets.r")

source("scripts/main_loop.r")

source("scripts/sna_analysis.r")

#source("scripts/make_tables.r")


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
