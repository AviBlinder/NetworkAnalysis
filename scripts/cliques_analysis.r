list_name <- cliques
idx <- 444
unwind_list <- function(list_name,idx){
  unwinded_vector <- unlist(list_name[[idx]])
  friend_number <- seq(1:length(unwinded_vector))
  unwinded_list <- data.frame()
  unwinded_list <- data.frame(cbind(idx,friend_number,sort(unwinded_vector)))
  unwinded_list$friends <- row.names(unwinded_list)
  row.names(unwinded_list) <- NULL
  unwinded_list$V3 <- NULL
  return (unwinded_list)
}



unw_list <- data.frame()
for (i in 1:length(cliques)){ 
  unw_list <- rbind(unw_list,unwind_list(cliques,i))
  
}

names(unw_list) <- c("clique_number","friend_number","friend")

head(unw_list)


w <- reshape(unw_list, 
             timevar = "friend_number",
             idvar = "clique_number",
             direction = "wide")

head(w[,2:4])
fr1t4 <- data.frame(apply(w[2:5], 1, paste, collapse=" "))
names(fr1t4) <- "friends1_to_4"
head(fr1t4)
sort(table(fr1t4),decreasing = TRUE)[1]
zdup_cliques <- data.frame(table(fr1t4))
names(dup_cliques)
hist(dup_cliques$Freq,breaks = c(0:40))
