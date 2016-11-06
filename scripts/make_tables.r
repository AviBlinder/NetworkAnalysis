#ftp://cran.r-project.org/pub/R/web/packages/gridExtra/vignettes/tableGrob.html
library(grid)
library(gridExtra)
library(formattable)
png(filename = "./figures/Top Reviewed Restaurants.png")
DT <- input_business[,2:3]
row.names(DT) <- NULL
names(DT) <- c("Name","Number of Reviews")
t1 <- ttheme_default()
grid.table(DT[1:10,], theme = t1)
dev.off()

###############################################################################
Sys.setlocale("LC_ALL","C")
png(filename = "./figures/Top Users by Number of Reviewes.png")
DT <- reviews_number1

names(DT) <-  c("User","Yelper Since","N. of Friends","Elite Years",
                "Number of Reviews on Top Restaurants","Total Reviews","Fans",
                "Average Stars")

DT <- DT[order(-DT$`N. of Friends`),]
row.names(DT) <- NULL

formattable(DT[1:15,], list(
  `N. of Friends` = color_tile("white","green"),
  'Number of Reviews on Top Restaurants' = color_bar(color = "lightgray")
))


dev.off()

###############################################################################
head(business_sna)
row.names(business_sna) <- NULL


formattable(business_sna[1:15,] )
#            , list(
#  `vcounts` = color_tile("white","green"),
#  'Number of Reviews on Top Restaurants' = color_bar(color = "lightgray")
#))



dev.off()
