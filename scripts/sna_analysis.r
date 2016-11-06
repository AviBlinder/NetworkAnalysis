##Steps in network analysis
#1. Descriptive state (vcount/ecount/degree/delimiter...)
#2. Vertex/Edge centrality (closeness, betweeness, eigenvector, edge_betweeness...
#3. Network Cohesion (cliques)
#4. Communities/clustering

##################################################################################
betweenesss_treshold <- 10

business_sna <- c()
users_betweeness <- c()
edge_betweeness <-  c()
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
  mean_betweeness <-  mean(btw_g_df$btw_grade)
  mean_betweeness
  btw_g_df[btw_g_df$btw_grade > mean_betweeness &
             btw_g_df$user_id %in% most_pop_user_df$user_id,]

  curr_business_btw <- data.frame(Business_Name = rep(current_business_name,
                                                      betweenesss_treshold),
                                  user_id = head(btw_g_df,betweenesss_treshold)[1],
                                  betweeness_score =
                                    head(btw_g_df,betweenesss_treshold)[2])

  users_betweeness <- rbind(users_betweeness,curr_business_btw)

  eb_Y_graph <- edge_betweenness(Y_graph)


  #Select the top n most important edges
  edgeB <- E(Y_graph)[order(-eb_Y_graph)][1:betweenesss_treshold]
 # attributes(edgeB)
  imp_edges <- attr(edgeB,"vnames")

  nodes_list <- c()
  for (i in 1:length(imp_edges)){

    node_left <- as.character(substring(imp_edges[i],1,22))
    node_right <- as.character(substring(imp_edges[i],24,nchar(imp_edges[i])))
    nodes <- cbind(node_left,node_right)
    nodes_list <- rbind (nodes_list,nodes)

  }
  nodes_list <- as.data.frame(nodes_list)
  nodes_list_unif <- as.character(nodes_list$node_left)
  nodes_list_unif <- c(nodes_list_unif,as.character(nodes_list$node_right))
  nodes_list_unif <- unique(nodes_list_unif)


  curr_business_edge_btw <- data.frame(Business_Name = rep(current_business_name,
                                                           length(nodes_list_unif)),
                                       user_id = nodes_list_unif)

  edge_betweeness <- rbind(edge_betweeness,curr_business_edge_btw)


}
saveRDS(object = business_sna,file = "data/business_sna")
saveRDS(object = users_betweeness,file="data/users_betweeness")
saveRDS(object = edge_betweeness,file="data/edge_betweeness")
#users_betweeness <- readRDS(file = "data/users_betweeness")
########################################################################################
#Analyzing the networks betweeness
names(users_betweeness)
head(users_betweeness,15)

library(dplyr)
ranks <- users_betweeness %>% group_by(Business_Name) %>%
     mutate(rank = dense_rank(btw_grade))
ranks_df <- data.frame(ranks)
users_betweeness$rank <- ranks_df$rank
head(users_betweeness)
user_ranks <- users_betweeness %>% group_by(user_id) %>%
  summarise(sum(rank))
user_ranks_df <- data.frame(user_ranks)
names(user_ranks_df) <- c("user_id","user_rank")
user_ranks_df <- user_ranks_df[order(-user_ranks_df$user_rank),]
head(user_ranks_df)
dim(most_pop_user_names_info)
most_pop_user_names_info <- merge(most_pop_user_names_info,user_ranks_df,
                                  by="user_id",all.x = FALSE,all.y = FALSE)

most_pop_user_names_info <- most_pop_user_names_info[order(-most_pop_user_names_info$user_rank),]
most_pop_user_names_info



#################
# users_betweeness_df <-as.data.frame(table(users_betweeness$user_id))
# head(users_betweeness_df)
# names(users_betweeness_df) <- c("user_id","BTW_Frequency")
# users_betweeness_df <- users_betweeness_df[order(-users_betweeness_df$BTW_Frequency),]
# head(users_betweeness_df)
# most_pop_user_names_info <- merge(most_pop_user_names_info,users_betweeness_df,by="user_id")
# most_pop_user_names_info <- most_pop_user_names_info[order(-most_pop_user_names_info$BTW_Frequency),]
# dim(users_betweeness_df)
# dim(most_pop_user_names_info)



#Analyzing the edge_betweeness in the networks
# edge_betweeness_users <- as.character(unique(edge_betweeness$user_id))
# setdiff(edge_betweeness_users,most_pop_user_names_info)
#
# edge_betweeness_df <- as.data.frame(table(edge_betweeness$user_id))
#
# row.names(edge_betweeness_df) <- NULL
# names(edge_betweeness_df) <- c("user_id","freq")
# edge_betweeness_df <- edge_betweeness_df[order(-edge_betweeness_df$freq),]
# head(edge_betweeness_df)
# edge_betweeness_df_not <- edge_betweeness_df[!edge_betweeness_df$user_id %in% most_pop_user_names_info$user_id,]
# head(edge_betweeness_df_not)
# edge_betweeness_df_not_full <- merge(edge_betweeness_df_not,users,by="user_id")
# edge_betweeness_df_not_full <- edge_betweeness_df_not_full[order(-edge_betweeness_df_not_full$freq),]
# head(edge_betweeness_df_not_full)




