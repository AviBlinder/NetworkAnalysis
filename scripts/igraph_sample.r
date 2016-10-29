#We can create a new graph object using the graph.data.frame() function
#From ?graph.data.frame, we can see that the function expects the first two columns of   d to specify the edges in the graph -- our edges object fits this description.
?graph.data.frame
##the vertices parameter expects a data frame where the first column is a
# vertex id and the remaining columns are properties of vertices in our graph.

actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
actors
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), 
                        advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)
plot(g)

## The opposite operation
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")
#our edges are undirected -- if A is a Facebook friend of B then B is a Facebook friend of A. Therefore, we set the directed parameter to FALSE.


