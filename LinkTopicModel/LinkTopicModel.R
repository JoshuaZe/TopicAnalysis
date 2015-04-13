# Important Core

storeList <- c("binetMaxCompart")
save(file = "LinkTopicModel.RData",list = storeList)
load(file = "LinkTopicModel.RData")
#####
# library required
#####
library(bipartite) #include library(sna)
library(linkcomm) #include library(igraph)
library(data.table)
library(plyr)
#####
# DATA
# B: Bipartite Network Matrix -> G: Co-term Network EdgeList
# paper-keyword -> keyword-keyword
#####
keywordsprojectingCotermNetwork <- list(keyword=colnames(binetMaxCompart),edges=projecting_tm(t(binetMaxCompart),method = "sum"))
keywordsprojectingCotermNetwork$edges <- as.data.frame(edge.duplicates(keywordsprojectingCotermNetwork$edges[,1:2])$edges,stringsAsFactors = F)
keywordsprojectingCotermNetwork$edges$id  <- 1:nrow(keywordsprojectingCotermNetwork$edges)
setnames(keywordsprojectingCotermNetwork$edges,1:2,c("i","j"))
storeList <- c(storeList,"keywordsprojectingCotermNetwork")
#####
# edges Similarity Calculation
# binetMaxCompart and keywordsprojectingCotermNetwork$edges
#####
edges <- keywordsprojectingCotermNetwork$edges
d_ply(edges,.(id),function(edge_a){
  e <- edges[which(edges$id>edge_a$id & (edges$i == edge_a$i | edges$i == edge_a$j | edges$j == edge_a$i | edges$j == edge_a$j)),]
  print(paste(edge_a$id,nrow(e),sep = "-"))
  if(nrow(e)!=0){
    d_ply(e,.(id),function(edge_b,edge_a){
      # similarity calculation of each
      simedge <- calculateSimilarity(edge_a,edge_b,binetMaxCompart)
      if(simedge!=0){
        #data.table(a_id=edge_a$id,b_id=edge_b$id,sim=simedge)
        #edges_b$id>edge_a$id
        write(c(edge_a$id,edge_b$id,simedge),"edgeSimilarity",ncolumns = 3,append = T,sep = "\t")
      }
    },edge_a,binetMaxCompart,.progress = "text")
  }
},edges,binetMaxCompart,.progress = "text")
similarity <- read.table(file = "edgeSimilarity",header = F,sep = "\t",col.names = c("a_id","b_id","sim"),stringsAsFactors = F)
# ranking similarity edge pair list (decrease)
similarity$rank <- length(similarity$sim) - rank(similarity$sim,ties.method = "max") + 1
similarity <- as.data.table(similarity)
keywordsprojectingCotermNetwork$edgeSimilarity <- similarity
#####
# link Community Tree Generation
# Link Topic Model
# edges : i j id
# similarity : a_id b_id sim rank
#####
keywordsprojectingCotermNetwork$LinkTopicTree <- linkCommunityTreeGeneration(keywordsprojectingCotermNetwork$edges,keywordsprojectingCotermNetwork$edgeSimilarity)
#####
# functions for algorithm
#####
calculateSimilarity <- function(edge_a,edge_b,binetmatrix){
  alpha <- 0.2
  beta <- 0.05
  #only Edges with one same node has similarity or zero
  if(((edge_a$i==edge_b$i)&(edge_a$j==edge_b$j))|((edge_a$i==edge_b$j)&(edge_a$j==edge_b$i))){
    # same edge
    return(1)
  }else if(edge_a$i==edge_b$i){
    # same i node : P(jk|i)
    return(((1-alpha)*length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j]*binetmatrix[,edge_b$j])!=0))+
              (alpha-beta)*min(length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j])!=0)),length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_b$j])!=0)),length(which((binetmatrix[,edge_a$j]*binetmatrix[,edge_b$j])!=0)))+
              beta*min(length(which(binetmatrix[,edge_a$i]!=0)),length(which(binetmatrix[,edge_a$j]!=0)),length(which(binetmatrix[,edge_b$j]!=0))))/length(which((binetmatrix[,edge_a$i])!=0)))
  }else if(edge_a$i==edge_b$j){
    # i node of a == j node of b : P(jk|i of a)
    return(((1-alpha)*length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j]*binetmatrix[,edge_b$i])!=0))+
              (alpha-beta)*min(length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j])!=0)),length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_b$i])!=0)),length(which((binetmatrix[,edge_a$j]*binetmatrix[,edge_b$i])!=0)))+
              beta*min(length(which(binetmatrix[,edge_a$i]!=0)),length(which(binetmatrix[,edge_a$j]!=0)),length(which(binetmatrix[,edge_b$i]!=0))))/length(which((binetmatrix[,edge_a$i])!=0)))
  }else if(edge_a$j==edge_b$i){
    # j node of a == i node of b : P(ik|j of a)
    return(((1-alpha)*length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j]*binetmatrix[,edge_b$j])!=0))+
              (alpha-beta)*min(length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j])!=0)),length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_b$j])!=0)),length(which((binetmatrix[,edge_a$j]*binetmatrix[,edge_b$j])!=0)))+
              beta*min(length(which(binetmatrix[,edge_a$i]!=0)),length(which(binetmatrix[,edge_a$j]!=0)),length(which(binetmatrix[,edge_b$j]!=0))))/length(which((binetmatrix[,edge_a$j])!=0)))
  }else if(edge_a$j==edge_b$j){
    # same j node : P(ik|j)
    return(((1-alpha)*length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j]*binetmatrix[,edge_b$i])!=0))+
              (alpha-beta)*min(length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j])!=0)),length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_b$i])!=0)),length(which((binetmatrix[,edge_a$j]*binetmatrix[,edge_b$i])!=0)))+
              beta*min(length(which(binetmatrix[,edge_a$i]!=0)),length(which(binetmatrix[,edge_a$j]!=0)),length(which(binetmatrix[,edge_b$i]!=0))))/length(which((binetmatrix[,edge_a$j])!=0)))
  }else{
    # no same node
    return(-1)
  }
}
linkCommunityTreeGeneration <- function(edges,similarity,rankNo=1){
  while((nrow(unique(edges[,ncol(edges),with=F]))!=1)&(rankNo <= max(similarity$rank))){
    # generate one layer of tree
    edges$tmp <- edges[,ncol(edges),with=F]
    setnames(edges,"tmp",paste("cluster",ncol(edges)-3,sep = "_"))
    edgepairs <- similarity[which(similarity$rank==rankNo),]
    edgepairs$id <- 1:nrow(edgepairs)
    print(paste(ncol(edges)-3,rankNo,nrow(edgepairs),"generation runing!",sep = "-"))
    # group of cluster combination
    d_ply(edgepairs,.(id),function(p,edges){
      e_a <- as.integer(edges[p$a_id,ncol(edges),with=F])
      e_b <- as.integer(edges[p$b_id,ncol(edges),with=F])
      set(edges,which(edges[,ncol(edges),with=F]==e_a),ncol(edges),min(e_a,e_b))
      set(edges,which(edges[,ncol(edges),with=F]==e_b),ncol(edges),min(e_a,e_b))
    },edges,.progress = "text")
    print(paste(ncol(edges)-3,rankNo,nrow(edgepairs),"generation finished!",sep = "-"))
    rankNo <- rankNo + nrow(similarity[which(similarity$rank==rankNo),])
  }
  write.table(edges,file = "edgeCommunityTree",quote = F,sep = "\t",row.names = F,col.names = T)
  return(edges)
}