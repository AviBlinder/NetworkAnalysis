#Read for datasets involved in analysis
start <- Sys.time()
friends <- read.csv("D:/Yelp/r_datasets/friends.csv",stringsAsFactors = FALSE)
users <- read.csv("D:/Yelp/r_datasets//users.csv",stringsAsFactors = FALSE)

business <- read.csv("D:/Yelp/r_datasets/business.csv")
reviews <- read.csv("D:/Yelp/r_datasets/reviews.csv",header = TRUE,stringsAsFactors = FALSE)
Sys.time() - start
##########################################################################################
