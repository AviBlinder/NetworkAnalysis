DT <- reviews_number1
library(grid)
library(gridExtra)
colnames(DT)[1] <- "User"
grid.draw(tableGrob(DT,
           cols = colnames(DT)[1:4], show.box = FALSE,
#                    name="Users Features",
                    separator="blue",
                    padding.v = unit(10, "mm"),
                    gpar.coretext = gpar(col = "orange", cex = 0.9),
                    padding.h = unit(4, "mm"),
                    gpar.corefill = gpar(fill = "white", col = "green")))


dev.off()
d <- head(iris, 3)
g <- tableGrob(d)
grid.newpage()
grid.draw(g)
dev.list()
?gtable_table

t1 <- ttheme_default(core=list(
  fg_params=list(fontface=c(rep("plain", 8), "bold.italic")),
  bg_params = list(fill=c(rep(c("grey95", "grey90"),
                              length.out=8), "#6BAED6"),
                   alpha = rep(c(1,0.5), each=5))
))

grid.table(DT[1:10, 1:8], theme = t1)

