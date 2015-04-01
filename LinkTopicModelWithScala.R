# only use R is so slow
# core part rewrite in Scala
#####
# DATA
# B: Bipartite Network Matrix -> G: Co-term Network EdgeList
# paper-keyword -> keyword-keyword
#####
keywordsprojectingCotermNetwork <- list(keyword=colnames(binetMaxCompart),edges=projecting_tm(t(binetMaxCompart),method = "sum"))
keywordsprojectingCotermNetwork$edges <- as.data.frame(edge.duplicates(keywordsprojectingCotermNetwork$edges[,1:2])$edges)
keywordsprojectingCotermNetwork$edges$id  <- 1:nrow(keywordsprojectingCotermNetwork$edges)
setnames(keywordsprojectingCotermNetwork$edges,1:2,c("i","j"))
#####
# Load DATA
#####
load(file = "LinkTopicModel.RData")
# paper-keyword matrix
write.table(binetMaxCompart,file = "BipartiteMaxCompartMatrix",quote = F,sep = "\t",row.names = F,col.names = F)
# i-j-id
write.table(keywordsprojectingCotermNetwork$edges,file = "cotermEdgeList",quote = F,sep = "\t",row.names = F,col.names = F)
