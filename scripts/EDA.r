dim(users)
round(prop.table(table(users$FriendsNumber == 0,dnn = "Prop. of Users with no friends")),2)
table(users$FriendsNumber == 0,dnn = "Prop. of Users with no friends")

table(users$FriendsNumber == 0,users$EliteYearsNumber,dnn=c("No Friends' User","Elite Years"))


table(users$fans > 0,users$EliteYearsNumber,dnn=c("No. Fans","Elite Years"))


##Elite Users
table(users$EliteYearsNumber > 0)
round(prop.table(table(users$EliteYearsNumber > 0)),2)

u1 <- users[users$FriendsNumber ==0,]
u2 <- users[users$FriendsNumber > 0,]
elite <- users[users$EliteYearsNumber >0 ,]
#Table of users with no friends - Check if are there any "Elite" users
table(u1$EliteYearsNumber,dnn = c("Elite Years"))
#Table of users with friends and compare "Elite Years"
table(u2$EliteYearsNumber,dnn = c("Elite Years"))
plot(u2$EliteYearsNumber,u2$FriendsNumber)
#
plot(users$yelping_since)
plot(elite$review_count,elite$EliteYearsNumber)
cols <- c("review_count","fans","average_stars","FriendsNumber","EliteYearsNumber")
plot(cor(users[,cols]))

#########
#The most important step to becoming an Elite Yelper is to write as many useful and unbiased reviews as possible. Make sure that your reviews are comprehensive but easy to read, and that they don’t contain very many glaring errors.
#Upload pictures of your food or of the service you are reviewing.
#Add tips to existing posts to help broaden the amount of information available
#Don’t play favorites with certain establishments. Try to write fairly about every place you go to.