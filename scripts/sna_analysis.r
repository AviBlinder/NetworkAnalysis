##Steps in network analysis
#1. Descriptive state (vcount/ecount/degree/delimiter...)
#2. Vertex/Edge centrality (closeness, betweeness, eigenvector, edge_betweeness...
#3. Network Cohesion (cliques)
#4. Communities/clustering

##################################################################################
# friends_b1 <- friends[friends$user_id %in% as.character(users_b1$user_id),]
# friends_b1$X <- NULL
# friends_b1_unique_userd_id <- as.character(unique(as.character(friends_b1$user_id)))
#
#
# Y_graph <- graph.data.frame(friends_b1,directed = FALSE,vertices = users_vertices)
#
#
# vcounts <- vcount(Y_graph);
# ecounts <- ecount(Y_graph)
# vcounts;ecounts
# #list.vertex.attributes(g)
# #list.edge.attributes(g)
# #head(V(g)$user_name)
# degree.g <- degree(Y_graph)
#
# degrees_table <- as.data.frame(table(degree.g))
# names(degrees_table) <- c("Degree","Frequency")
# row.names(degrees_table) <- NULL
# degrees_table$Degree <- as.integer(degrees_table$Degree)
#
# p1 <- ggplot(degrees_table, aes(x = Degree, y = Frequency))
# plot1 <- p1 +  geom_point(aes(color = Degree)) +
#   geom_vline(xintercept = 5, color="red",show.legend = FALSE) +
#   labs(x="Degree Distribution",
#        y = "Frequency") +
#   ggtitle("Degrees Distribution")
#
#
# p1 <- ggplot(degrees_table, aes(x = log10(Degree), y = log10(Frequency)))
# plot2 <- p1 +  geom_point(aes(color = log10(Degree),show.legend = FALSE)) +
#   geom_vline(xintercept = log10(5), color="red",show.legend = FALSE) +
#   labs(x="Log10 - Degree Distribution",
#        y = "Log10 - Frequency")  +
#   ggtitle("Log10 of Degrees Distribution")
#
# require(gridExtra)
# grid.arrange(plot1, plot2, ncol=2)
#
#
# #Network diameter
# net_diameter <- diameter(Y_graph)
# net_diameter
# farthest_users <-  farthest.nodes(Y_graph,directed = F)$vertices
# farthest_users[[1]]$user_name;  farthest_users[[2]]$user_name
#
#
# graphDensity <- graph.density(Y_graph)
# graphDensity
#
#
##################################################################################

betweenesss_treshold <- 10

business_sna <- c()
business_betweeness <- c()
library(igraph)
i <- 1
for (i in 1:nrow(selected_businesses)){
  current_business <- as.character(selected_businesses$business_id[i])
  current_business_name <- as.character(selected_businesses$name[i])
  cat("current business = ", current_business_name, "\n")
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
  vcounts;ecounts
#list.vertex.attributes(g)
#list.edge.attributes(g)
#head(V(g)$user_name)
  max_degree.g <- max(table(degree(Y_graph)))


#Network diameter
  net_diameter <- diameter(Y_graph)
  net_diameter
  farthest_users <-  farthest.nodes(Y_graph,directed = F)$vertices
  farthest_users[[1]]$user_name;  farthest_users[[2]]$user_name


    graphDensity <- graph.density(Y_graph)
    graphDensity


    curr_business_sna <- data.frame(Business_Name = current_business_name,
                                    vcounts = vcounts,
                                    ecounts = ecounts,
                                    MaxGraphdegree = max_degree.g,
                                    net_diameter = net_diameter,
                                    graphDensity = graphDensity)

      business_sna <- rbind(business_sna,curr_business_sna)

      btw_grade <- betweenness(Y_graph)
      head(sort(btw_grade,decreasing = T))
      btw_g_df <- as.data.frame(btw_grade)
      btw_g_df$user_id <- row.names(btw_g_df)
      row.names(btw_g_df) <- NULL
      btw_g_df <- btw_g_df[,c(2:1)]
      btw_g_df <- btw_g_df[order(-btw_g_df$btw_grade),]
      head(btw_g_df)
      mean_betweeness <-  mean(btw_g)
      mean_betweeness
      btw_g_df[btw_g_df$btw_grade > mean_betweeness &
                 btw_g_df$user_id %in% most_pop_user_df$user_id,]
      head(btw_g_df,10)


      curr_business_btw <- data.frame(Business_Name = rep(current_business_name,betweenesss_treshold),
                                      user_id = head(btw_g_df,betweenesss_treshold)[1],
                                      betweeness_score = head(btw_g_df,betweenesss_treshold)[2])

      business_betweeness <- rbind(business_betweeness,curr_business_btw)
}


#Average degree of the neighbors of a given vertex
#Beyond the degree distribution itself,it can be interesting
# to understand the manner in which vertices of different degrees are linked with
# each other.
# Useful in assessing this characteristic is the notion of the average degree of the
# neighbors of a given vertex.
# For example, a plot of average neighbor degree versus vertex degree , suggests that
# while there is a tendency for vertices of higher degrees to link with similar vertices,
# vertices of lowerd egree tend to link with vertices of both lower and higher degrees.

degree.g <- degree(Y_graph)
knn.deg.g <- graph.knn(Y_graph,V(Y_graph))$knn




###########################################
##Vertex centrality (closeness, betweeness, eigenvector centrality)
  # cls_g <- closeness(Y_graph)
  # head(sort(cls_g,decreasing = T))
  #
  # cls_g_df <- as.data.frame(cls_g)
  # cls_g_df$user_id <- row.names(cls_g_df)
  # row.names(cls_g_df) <- NULL
  #
  # mean_closeness <- mean(cls_g)
  # cls_g_df[cls_g_df$cls_g > mean_closeness & cls_g_df$user_id %in% most_pop_user_df$user_id,]


  #find shortest_path between elite user that gave high rate and elite user that gave low rate
  #shortest_paths(g,"kGgAARL2UmvCcTRfiscjug","YRnHZmBUC2MDOUN38hLLVg",output="epath")$epath[1]
  #dist_g <- distances(g)
  #dist_table <- distance_table(g,directed = FALSE)

  #eb_Y_graph <- edge_betweenness(Y_graph)
  #head(eb_Y_graph)

  #edgeB <- E(Y_graph)[order(-eb_Y_graph)][1:10]
  #edgeB
  #V(Y_graph) [head(edgeB)]

################
####
#Analyzing the networks betweeness
names(business_betweeness)


###Network Cohesion
#Clique = complete subgraph
cliques(Y_graph) [sapply(cliques(Y_graph),length) > 4]
max(sapply(cliques(Y_graph),length))

table(sapply(maximal.cliques(Y_graph),length))
max_maximal_clique <- max(sapply(maximal.cliques(Y_graph),length))



############################
##Community Analysis
##Clustering
#sgc <- spinglass.community(Y_graph)

## clusters --> fast algorithm
#cls <- clusters(Y_graph)
##edge.betweenness.community --> heavy algorithm!!
#cls <- edge.betweenness.community(Y_graph)

#fastgreedy --> fast algorithm
cls <- fastgreedy.community(Y_graph)

table(cls$membership)
cls$csize
cls$no

V(Y_graph)$membership <- cls$membership
V(Y_graph) [ membership == 1 ]$color <- "cyan"
V(Y_graph) [ membership == 2 ]$color <- "green"
V(Y_graph) [ membership == 3 ]$color <- "blue"
V(Y_graph) [ membership == 4 ]$color <- colors()[4]
V(Y_graph) [ membership == 5 ]$color <- colors()[5]
V(Y_graph) [ membership == 6 ]$color <- colors()[6]
V(Y_graph) [ membership == 7 ]$color <- colors()[7]
V(Y_graph) [ membership == 8 ]$color <- colors()[8]
V(Y_graph) [ membership == 9 ]$color <- colors()[9]
V(Y_graph) [ membership > 9 ]$color <- colors()[10]
V(Y_graph)$size <- 0.1

set.seed(4312)
l <- layout.drl(Y_graph)
plot(Y_graph,
     #     layout=layout.fruchterman.reingold,
     layout=l,
     vertex.color=V(Y_graph)$color,
     vertex.size = V(Y_graph)$size,
     vertex.label = NA,
     edge.width = NA,
     edge.arrow.size = 0.001)


