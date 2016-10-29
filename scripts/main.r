rm(list=ls())

users <- read.csv("D:/Yelp/YelpChallenge2016_scripts/data/users.csv")
friends <- read.csv("D:/Yelp/YelpChallenge2016_scripts/data/friends.csv")
head(users$FriendsNumber,16)

head(friends);
unique_users_total <- as.vector(unique(users$user_id))
#   
unique_users <- as.vector(unique(friends$user_id))
#unique_friends <- as.vector(unique(friends$friends))
head(unique_friends);head(unique_users)

##Handling Yelping_since variable
library(lubridate)
users$yelping_since <- ymd(paste(users$yelping_since,"01",sep = '-'))
seniority_beg <- max(users$yelping_since) + months(1)
seniority_beg


#Calculating "seniority" in Yelp. Starting date = 2016-08-01 (Max Date + 1 month)
users$seniorityMonths <- round(as.numeric(seniority_beg - users$yelping_since) / 30)
head(users)

#node_users <- unique(c(unique_users,unique_friends))
node_users <- data.frame(user_id = unique_users)
head(node_users)

node_users_w_props <- merge(node_users,users,by="user_id")
head(node_users_w_props)
write.csv(node_users,"data/node_users.csv",row.names = FALSE)
write.csv(node_users_w_props,"data/node_users_w_props.csv",row.names = FALSE)

write.csv(friends,"data/relation_friends.csv",row.names = FALSE)
#########################################################################

node_users_w_props <- read.csv("data/node_users_w_props.csv")
names(node_users_w_props)
