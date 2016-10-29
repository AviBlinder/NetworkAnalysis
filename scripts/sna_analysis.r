##Steps in network analysis
#1. Descriptive state (vcount/ecount/degree/delimiter...)
#2. Vertex/Edge centrality (closeness, betweeness, eigenvector, edge_betweeness...
#3. Network Cohesion (cliques)
#4. Communities/clustering

business_sna <- c()
library(igraph)
i <- 1
for (i in 1:nrow(selected_businesses)){
  current_business <- as.character(selected_businesses$business_id[i])
  cat("current business = ", current_business, "\n")
  r1 <- subset(reviews,
             reviews$business_id == current_business)

  current_business
  length(unique(r1$user_id))


  b1 <-  subset(business,business$business_id == current_business)


#Isolate all the friends related to the users that gave reviews to the business
  users_b1 <- data.frame(unique(r1$user_id))
  names(users_b1) <- "user_id"
  dim(users_b1)

  friends_b1 <- friends[friends$user_id %in% as.character(users_b1$user_id),]
  friends_b1$X <- NULL
  friends_b1_unique_userd_id <- as.character(unique(as.character(friends_b1$user_id)))


  friends_b1_unique_friends <- as.character(unique(friends_b1$friends))
  vertices <- unique(c(friends_b1_unique_userd_id,friends_b1_unique_friends))
  length(vertices)


  users_vertices <- users[users$user_id %in% vertices,]
  dim(users_vertices)
  users_vertices$user_name <- users_vertices$name
  users_vertices$name <- NULL
  users_vertices$user_id <- as.character(users_vertices$user_id)

  cols_order <- c("user_id", "user_name",
                "yelping_since","review_count","fans","average_stars","FriendsNumber",
          "EliteYearsNumber")
  users_vertices <- users_vertices[,cols_order]
  head(users_vertices)

  friends_b1$user_id <- as.character(friends_b1$user_id)
  friends_b1$friends <- as.character(friends_b1$friends)
#users_vertices[users_vertices$user_id %in% dup_users,]
#friends_b1[friends_b1$friends %in% dup_users,]
#friends_b1[friends_b1$user_id %in% dup_users,]

#subset(friends_b1,as.character(friends_b1$user_id) == "NXbrKqnF20Wfvu51Z3pXtw")
###
#Create a graph dataframe
  Y_graph <- graph.data.frame(friends_b1,directed = FALSE,vertices = users_vertices)
#g <- graph.data.frame(friends_b1,directed = FALSE)
  is.simple(Y_graph)
  dup_users <- V(Y_graph)$name[which_multiple(Y_graph, eids = E(Y_graph))]
  dup_users

  Y_graph <- simplify(Y_graph)
  is.simple(Y_graph)
#Basic Summary Statistics
#Number of vertices(nodes) and number of relationships

  vcounts <- vcount(Y_graph);
  ecounts <- ecount(Y_graph)

#list.vertex.attributes(g)
#list.edge.attributes(g)
#head(V(g)$user_name)
  degree.g <- degree(Y_graph)

  # hist(degree.g[degree.g < 4],col="blue",
  #    xlab="Degree", ylab="Frequency",
  #    main="Degree Distribution")
  #
  # hist(degree.g[degree.g >= 4],col="blue",
  #    xlab="Degree", ylab="Frequency",
  #    xlim=c(4,350),
  #    main="Degree Distribution")

#Network diameter
  net_diameter <- diameter(Y_graph)

  #farthest.nodes(Y_graph,directed = F)
#Average degree of the neighbors of a given vertex
#Beyond the degree distribution itself,it can be interesting
# to understand the manner in which vertices of different degrees are linked with
# each other.
# Useful in assessing this characteristic is the notion of the average degree of the
# neighbors of a given vertex.
# For example, a plot of average neighbor degree versus vertex degree , suggests that
# while there is a tendency for vertices of higher degrees to link with similar vertices,
# vertices of lowerd egree tend to link with vertices of both lower and higher degrees.

#knn.deg.g <- graph.knn(Y_graph,V(Y_graph))$knn
#plot(degree.g,knn.deg.g,log="xy",
#     col=colors()[35],
#     xlab=c("Log Vertex Degree"),
#     ylab=c("Log Average Neighbor Degree"))

    graphDensity <- graph.density(Y_graph)

    curr_business_sna <- data.frame(current_business = current_business,
                                    vcounts = vcounts,
                                    ecounts = ecounts,
                                    Graphdegree = degree.g,
                                    net_diameter = net_diameter,
                                    graphDensity = graphDensity)

    business_sna <- rbind(business_sna,curr_business_sna)
}
###########################################
##Vertex centrality (closeness, betweeness, eigenvector centrality)
btw_g <- betweenness(Y_graph)
head(sort(btw_g,decreasing = T))
btw_g_df <- as.data.frame(btw_g)
btw_g_df$user_id <- row.names(btw_g_df)
row.names(btw_g_df) <- NULL
mean_betweeness <-  mean(btw_g)
btw_g_df[btw_g_df$btw_g > mean_betweeness & btw_g_df$user_id %in% most_pop_user_df$user_id,]


cls_g <- closeness(Y_graph)
head(sort(cls_g,decreasing = T))

cls_g_df <- as.data.frame(cls_g)
cls_g_df$user_id <- row.names(cls_g_df)
row.names(cls_g_df) <- NULL

mean_closeness <- mean(cls_g)
cls_g_df[cls_g_df$cls_g > mean_closeness & cls_g_df$user_id %in% most_pop_user_df$user_id,]


#find shortest_path between elite user that gave high rate and elite user that gave low rate
#shortest_paths(g,"kGgAARL2UmvCcTRfiscjug","YRnHZmBUC2MDOUN38hLLVg",output="epath")$epath[1]
#dist_g <- distances(g)
#dist_table <- distance_table(g,directed = FALSE)

eb_Y_graph <- edge_betweenness(Y_graph)
head(eb_Y_graph)
E(Y_graph)[order(-eb_Y_graph)][1:10]
################
###Network Cohesion
#Clique = complete subgraph
cliques(Y_graph) [sapply(cliques(Y_graph),length) > 4]
max(sapply(cliques(Y_graph),length))

table(sapply(maximal.cliques(Y_graph),length))
max(sapply(maximal.cliques(Y_graph),length))



############################
##Community Analysis

#This algorithm is the Clauset-Newman-Moore algorithm. In this case the algorithm is
# agglomerative and at each step the merge is decided by the optimization of modularity that
# it produces as the result of the merge.
# This is very fast, but has the disadvantage of being a greedy algorithm, so it
# is might not produce the best overall community partitioning, although I find it
# very useful and very accurate.
fg_comms <- fastgreedy.community(Y_graph)
head(fg_comms)
#fg_comms$names
##clusters
cluster <- fg_comms$membership
cluster

#Number of clusters
length(fg_comms)
sizes_df <- data.frame(sizes(fg_comms))
names(sizes_df) <- c("cluster_id","MembersNumber")
sort(sizes(fg_comms),decreasing = T)
subset(sizes_df,sizes_df$MembersNumber == 2)
membership(fg_comms)[fg_comms$membership == 135]
cluster_id <- unlist(subset(sizes_df,sizes_df$MembersNumber > 100,select="cluster_id"))

V(Y_graph)$clusterId <- fg_comms$membership
Y_graph_sub <- V(Y_graph)[V(Y_graph)$clusterId %in% as.integer(cluster_id)]


## Run Girvan-Newman clustering algorithm.
communities = edge.betweenness.community(Y_graph)
head(communities)
# Extract cluster assignments and merge with nodes data.frame.
memb = data.frame(name = communities$names, cluster = communities$membership)


#############################
##Spin-glass community detection:
m1 <- membership(Y_graph)
sgc <- spinglass.community(Y_graph)
sgc <- cluster_spinglass(Y_graph)
V(Y_graph)$membership <- sgc$membership
# found 4 communities 0, 1, 2, 3
V(g) [ membership == 0 ]$color <- "cyan"
V(g) [ membership == 1 ]$color <- "green"
V(g) [ membership == 2 ]$color <- "blue"
V(g) [ membership == 3 ]$color <- "red"
V(g)$size <- 4
V(g) [ name == "the-life-scientists" ]$size <- 20
png(filename = "tls.png", height = 800, width = 800)
plot(g, layout=layout.fruchterman.reingold, vertex.color=V(g)$color, vertex.size = V(g)$size, vertex.label = NA, edge.arrow.size = 0.5)
dev.off()
