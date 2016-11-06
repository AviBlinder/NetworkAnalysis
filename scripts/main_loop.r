###Proportion of Yelps users having at least one friend
round (prop.table(table(users$FriendsNumber > 0)),2)


#Summarize reviews table (e.g. number of reviews each business received)
tb1 <- data.frame(table(reviews$business_id))
names(tb1) <- c("business_id","reviewsNumber")

##
number_of_businesses <- 100
analysis_treshold <- 0.30

input_business <- head(tb1[order(-tb1$reviewsNumber),],number_of_businesses)
input_business <- merge(input_business,business[,c("business_id","name")],by="business_id")

range(input_business$reviewsNumber)
input_business <- input_business[,c(1,3,2)]
input_business <- input_business[order(-input_business$reviewsNumber),]

###########################################################################################

##Analyze the number of users with and withoud friends that gave reviews to a business
friends_props <- c()
for (i in 1:nrow(input_business)){
  # Pick all the reviews from a specific business, containing any rate
  current_business <- input_business$business_id[i]
  r1 <- subset(reviews,
               reviews$business_id == current_business)


  #r2 <- subset(reviews,
  #     reviews$business_id == max_business & reviews$stars >= 4)
  #Pick the information about the business from the business dataset
  b1 <-  subset(business,business$business_id == as.character(current_business))

  #Pick all the users that gave a review to that business
  users_b1 <- data.frame(unique(r1$user_id))
  names(users_b1) <- "user_id"

  ##How many users have at least one friend
  unique_users_b1 <- users[users$user_id %in% users_b1$user_id,]
  props <- round(prop.table(table(unique_users_b1$FriendsNumber == 0)),2)
  #Convert the table into a 2-cols dataframe
  props_df <- data.frame(FALSES = props[1],TRUES = props[2])
  row.names(props_df) <- NULL
  barplot(props,col=colors()[i+20])

  props_business <- cbind(props_df,business_id=as.character(current_business))

  friends_props <- rbind(friends_props,props_business)

}

friends_props[order(-friends_props$TRUES),]

businesses_for_analysis <- friends_props[friends_props$TRUES >= analysis_treshold,]
businesses_for_analysis
businesses_for_analysis <- businesses_for_analysis[order(-businesses_for_analysis$TRUES),]

selected_businesses <- merge(businesses_for_analysis,business,by="business_id")
selected_businesses <- selected_businesses[order(-selected_businesses$TRUES),]
#View(selected_businesses)
dim(businesses_for_analysis)
saveRDS(object = selected_businesses,
        file="/DataScienceProjects/NetworkAnalysis/data/selected_businesses")

#names(selected_businesses)
reviews_table <- reviews[(reviews$business_id %in% selected_businesses$business_id),]

t1 <- table(as.character(reviews_table$user_id))
most_pop_user <- head(sort(t1,decreasing = TRUE),50)
most_pop_user
most_pop_user_df <- data.frame(most_pop_user)
names(most_pop_user_df) <- c("user_id","Frequency")
reviews_number <- as.data.frame(most_pop_user)
names(reviews_number) <- c("user_id","AppearanceNumber")
reviews_number



############################################################################################
most_pop_user_names <- reviews_number$user_id

most_pop_user_names_info <- users[users$user_id %in% most_pop_user_names,]
most_pop_user_names_info <- merge(most_pop_user_names_info,reviews_number,by="user_id")
most_pop_user_names_info <- most_pop_user_names_info[order(-most_pop_user_names_info$AppearanceNumber),]
#View(most_pop_user_names_info)
saveRDS(object = most_pop_user_names_info,
        file = "/DataScienceProjects/NetworkAnalysis/data/most_pop_user_names_info")


reviews_number1 <- most_pop_user_names_info[,-c(1,7)]
row.names(reviews_number1) <- NULL
reviews_number1 <- reviews_number1[,c(3,1,6,7,8,2,4,5)]
names(reviews_number1) <- c("name","Yelper Since","Number of Friends","Elite Years",
                            "Number of Reviews on Top Restaurants","Total Reviews","Fans",
                            "Average Stars")
#########################################################################################
# business_featuers <- names(selected_businesses)[c(1,31:87)]
# idx <- which(business_featuers %in% c("Noise.Level","Attire","Alcohol",
#                                       "Price.Range","BYOB.Corkage",
#                                       "Smoking","Wi.Fi","Ages.Allowed"))
# business_featuers <- business_featuers[-idx]
# selected_business_features <- selected_businesses[,business_featuers]
# names(selected_business_features)
# sapply(selected_businesses,class)
#
# dim(selected_business_features)
# sort(colSums(selected_business_features[2:50]),decreasing = TRUE)
#
# idx <- which(names(selected_businesses) %in% c("business_id","BYOB"))
# selected_businesses_lm <- selected_businesses[,-idx]
# #fit1 <- lm(selected_businesses_lm$TRUES ~ . ,data=selected_businesses_lm)
# sapply(selected_businesses_lm,length(levels))
# names(selected_businesses_lm)
