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
################################################################################
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


plot(degree.g,knn.deg.g,log="xy",
     col=colors()[35],
     xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))
names(degree.g) <- NULL
names(knn.deg.g) <- NULL

knn_degrees <- data.frame(x=degree.g, y=knn.deg.g)
head(knn_degrees)
png(filename = "./figures/Vertex Degrees vs. Neighbors Degrees.png")
p1 <- ggplot(knn_degrees, aes(x = log(x), y = log(y)))
p1 +  geom_point(aes(color = log(x),show.legend = FALSE)) +
  scale_colour_gradient(low = colors()[121],high = "blue") +
  labs(x="Log Vertex Degree",
       y = "Log Neighbor Degree")  +
  ggtitle("Vertex Degrees vs. Neighbors Degrees (Log Scaled)")

dev.off()
################################################################################
