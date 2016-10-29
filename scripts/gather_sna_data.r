##Steps in network analysis
#1. Descriptive state (vcount/ecount/degree/delimiter...)
#2. Vertex/Edge centrality (closeness, betweeness, eigenvector, edge_betweeness...
#3. Network Cohesion (cliques)
#4. Communities/clustering

business_sna <- c()
betweeness_pop_users_df <- data.frame()
betweeness_high_users_df <- data.frame()
library(igraph)
i <- 1
for (i in 1:nrow(selected_businesses)){
  current_business <- as.character(selected_businesses$business_id[i])
  cat("current business = ", current_business, "-->", i ,  "\n")
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
  if(!is.simple(Y_graph)){
    dup_users <- V(Y_graph)$name[which_multiple(Y_graph, eids = E(Y_graph))]
    dup_users

    Y_graph <- simplify(Y_graph)
  }
    cat("simple : ", is.simple(Y_graph),"\n")
  #Basic Summary Statistics
  #Number of vertices(nodes) and number of relationships

  vcounts <- vcount(Y_graph);
  ecounts <- ecount(Y_graph)

  #degree.g <- degree(Y_graph)

  #Network diameter
  net_diameter <- diameter(Y_graph)


  graphDensity <- graph.density(Y_graph)

  ##
  ##Vertex centrality (closeness, betweeness, eigenvector centrality)
  btw_g <- betweenness(Y_graph)
  head(sort(btw_g,decreasing = T))
  btw_g_df <- as.data.frame(btw_g)
  btw_g_df$user_id <- row.names(btw_g_df)
  row.names(btw_g_df) <- NULL
  mean_betweeness <-  mean(btw_g)

  betweeness_pop_users <- cbind(current_business,btw_g_df[btw_g_df$btw_g > mean_betweeness & btw_g_df$user_id %in% most_pop_user_df$user_id,]$user_id)

  betweeness_high_users <- cbind(current_business,head(btw_g_df[order(-btw_g_df$btw_g),]$user_id,5))

  cls_g <- closeness(Y_graph)
  head(sort(cls_g,decreasing = T))

#  cls_g_df <- as.data.frame(cls_g)
#  cls_g_df$user_id <- row.names(cls_g_df)
#  row.names(cls_g_df) <- NULL

#  mean_closeness <- mean(cls_g)
#  cls_g_df[cls_g_df$cls_g > mean_closeness & cls_g_df$user_id %in% most_pop_user_df$user_id,]
#
# Network Cohesion

    max_maximal_clique <- max(sapply(maximal.cliques(Y_graph),length))

  curr_business_sna <- data.frame(current_business = current_business,
                                  vcounts = vcounts,
                                  ecounts = ecounts,
   #                               Graphdegree = degree.g,
                                  net_diameter = net_diameter,
                                  graphDensity = graphDensity,
                                  max_clique = max_maximal_clique
                                  )



  business_sna <- rbind(business_sna,curr_business_sna)
  if(dim(betweeness_pop_users)[2]>1){
      betweeness_pop_users_df <- rbind(betweeness_pop_users_df,betweeness_pop_users)
  }
  betweeness_high_users_df <- rbind(betweeness_high_users_df,betweeness_high_users)

}
#########################################################################################
business_sna
head(betweeness_pop_users_df)
names(betweeness_pop_users_df) <- c("business_id","user_id")
betweeness_pop_users_df$type <- "popular_user"
names(betweeness_high_users_df) <- c("business_id","user_id")
betweeness_high_users_df$type <- "high_centrality_user"
high_users <- rbind(betweeness_pop_users_df,betweeness_high_users_df)
head(high_users)
sort(table(high_users$user_id),decreasing = T)[1:4]

subset(high_users,high_users$user_id == "9A2-wSoBUxlMd3LwmlGrrQ")
subset(high_users,high_users$user_id == "k5p3YP1ZjCa8ZS3xqXgBug")
subset(high_users,high_users$user_id == "OAd-vbR_POac1zHtu-Y2Zg")
subset(high_users,high_users$user_id == "5lq4LkrviYgQ4LJNsBYHcA")

sort(table(betweeness_high_users_df$user_id),decreasing = T)[1:4]
