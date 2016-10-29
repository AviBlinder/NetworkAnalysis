##Plot frequencies of reviews

par(mfrow=c(2,2))
#png(filename = "D:/DataScienceProjects/NetworkAnalysis/Reviews Histogram.png")
#    width = 800,height = 800)
hist(tb1$reviewsNumber[tb1$reviewsNumber <=100],
     col="coral1",
     main= "",
     xlab = "Number of reviews"
)
hist(tb1$reviewsNumber[tb1$reviewsNumber > 100 & tb1$reviewsNumber <= 600],
     col="coral2",
     xlim = c(100,600),
     main= "",
     xlab = "Number of reviews")
hist(tb1$reviewsNumber[tb1$reviewsNumber > 600 & tb1$reviewsNumber <= 1000],
     col="coral3",
     xlim = c(600,1000),
     main= "",
     xlab = "Number of reviews")
hist(tb1$reviewsNumber[tb1$reviewsNumber > 1000 ],
     col="coral4",
     xlim = c(1000,6000),
     main= "",
     xlab = "Number of reviews")
#dev.off()
par(mfrow=c(1,1))

