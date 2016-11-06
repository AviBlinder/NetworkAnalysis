#ftp://cran.r-project.org/pub/R/web/packages/gridExtra/vignettes/tableGrob.html
library(grid)
library(gridExtra)
library(formattable)
###############################################################################
png(filename = "./figures/Degrees Distributions.png")
degree.g <- degree(Y_graph)
degrees_table <- as.data.frame(table(degree.g))
names(degrees_table) <- c("Degree","Frequency")
row.names(degrees_table) <- NULL
degrees_table$Degree <- as.integer(degrees_table$Degree)

p1 <- ggplot(degrees_table, aes(x = Degree, y = Frequency))
plot1 <- p1 +  geom_point(aes(color = Degree)) +
  geom_vline(xintercept = 5, color=colors()[230],show.legend = FALSE) +
  labs(x="Degree Distribution",
       y = "Frequency") +
  ggtitle("Degrees Distribution")


p1 <- ggplot(degrees_table, aes(x = log10(Degree), y = log10(Frequency)))
plot2 <- p1 +  geom_point(aes(color = log10(Degree),show.legend = FALSE)) +
  geom_vline(xintercept = log10(5), color=colors()[230],show.legend = FALSE) +
  labs(x="Log10 - Degree Distribution",
       y = "Log10 - Frequency")  +
  ggtitle("Log10 of Degrees Distribution")

require(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

dev.off()


###############################################################################
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
