#ftp://cran.r-project.org/pub/R/web/packages/gridExtra/vignettes/tableGrob.html
library(grid)
library(gridExtra)

DT <- input_business[,2:3]
row.names(DT) <- NULL

t1 <- ttheme_default()
grid.table(DT[1:10,], theme = t1)

###############################################################################
DT <- reviews_number1
row.names(DT) <- NULL
names(DT) <-  c("User","Yelper Since","N. of Friends","Elite Years",
                "Number of Reviews on Top Restaurants","Total Reviews","Fans",
                "Average Stars")


t1 <- ttheme_default()
grid.table(DT[,c(1:3,6)], theme = t1)



###############################################################################
t1 <- ttheme_default(core=list(
  fg_params=list(fontface=c(rep("plain", 8), "bold.italic")),
  bg_params = list(fill=c(rep(c("grey95", "grey90"),
                              length.out=8), "#6BAED6"),
                   alpha = rep(c(1,0.5), each=5))
))

grid.table(DT[1:10, 1:8], theme = t1)
t1 <- ttheme_default()
grid.table(DT[1:10, 1:8], theme = t1)

###############################################################################
business_sna
DT <- business_sna
row.names(DT) <- NULL
dim(DT)
t1 <- ttheme_default()
t1 <- ttheme_default(core=list(
  fg_params=list(cex = 0.8,hjust=1, x=0.5,fontface=c(rep("plain", 6), "bold.italic"))),
  colhead = list(fg_params=list(cex = 0.8))
)
#  bg_params = list(fill=c(rep(c("grey95", "grey90"),
#                              length.out=6), "#6BAED6"),
#                   alpha = rep(c(1,0.5), each=5))


png(filename = "business_sna.png")

g <- grid.table(DT, theme = t1)


dev.off()
