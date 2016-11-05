DT <- reviews_number1
library(grid)
library(gridExtra)
colnames(DT)[1] <- "User"
grid.draw(tableGrob(DT))
          , cols = colnames(DT)[1:4], show.box = FALSE,
                    name="test",
                    separator="blue",
                    padding.v = unit(10, "mm"),
                    gpar.coretext = gpar(col = "orange", cex = 0.9),
                    padding.h = unit(4, "mm"),
                    gpar.corefill = gpar(fill = "white", col = "green")))

