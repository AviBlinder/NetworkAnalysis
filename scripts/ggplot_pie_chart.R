head(selected_businesses)
names(selected_businesses)
selected_businesses_plot <- selected_businesses[,c("name","FALSES","TRUES")]

#                                                   "city","review_count")]
row.names(selected_businesses_plot) <- NULL
selected_businesses_plot

selected_long <- tidyr::gather(selected_businesses_plot, "name")
names(selected_long) <- c("name","Property","Perc")
selected_long <- selected_long[order(selected_long$name),]
names(selected_long)
library(ggplot2)
# Barplot
bp<- ggplot(selected_long[1:2,], aes(x="", y=Perc, fill=Property))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie


pie + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
pie + scale_fill_brewer(palette="Dark2")
pie + scale_fill_brewer(palette="Blues")+
  theme_minimal()
pie + scale_fill_grey() + theme_minimal()


theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3  ),
                label = (Property/100), size=5)

##########
League<-c("A","B","A","C","D","E","A","E","D","A","D")
data<-data.frame(League) # I have more variables
data
data$League <- reorder(data$League, X = data$League, FUN = function(x) -length(x))
data$League
at <- nrow(data) - as.numeric(cumsum(sort(table(data)))-0.5*sort(table(data)))
cumsum(sort(table(data)))
nrow(data)
nrow(data) - cumsum(sort(table(data)))
0.5*sort(table(data))

round(sort(table(data))/sum(table(data)),2)*100
label=paste0(round(sort(table(data))/sum(table(data)),2) * 100,"%")

p <- ggplot(data,aes(x="", fill = League,fill=League)) +
  geom_bar(width = 1) +
  coord_polar(theta="y") +
  annotate(geom = "text", y = at, x = 1, label = label)
p
