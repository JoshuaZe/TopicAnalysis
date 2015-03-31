library(plyr)
library(data.table)
# old
edgeSimilarity <- function(edge_a,edge_b,binetmatrix){
  #only Edges with one same node has similarity or zero
  if(((edge_a$i==edge_b$i)&(edge_a$j==edge_b$j))|((edge_a$i==edge_b$j)&(edge_a$j==edge_b$i))){
    # same edge
    return(1)
  }else if(edge_a$i==edge_b$i){
    # same i node : P(jk|i)
    return(length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j]*binetmatrix[,edge_b$j])!=0))/length(which((binetmatrix[,edge_a$i])!=0)))
  }else if(edge_a$i==edge_b$j){
    # i node of a == j node of b : P(jk|i of a)
    return(length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j]*binetmatrix[,edge_b$i])!=0))/length(which((binetmatrix[,edge_a$i])!=0)))
  }else if(edge_a$j==edge_b$i){
    # j node of a == i node of b : P(ik|j of a)
    return(length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j]*binetmatrix[,edge_b$j])!=0))/length(which((binetmatrix[,edge_a$j])!=0)))
  }else if(edge_a$j==edge_b$j){
    # same j node : P(ik|j)
    return(length(which((binetmatrix[,edge_a$i]*binetmatrix[,edge_a$j]*binetmatrix[,edge_b$i])!=0))/length(which((binetmatrix[,edge_a$j])!=0)))
  }else{
    # no same node
    return(-1)
  }
}
edgeSimilarity_patch <- function(edge_a,edge_b,binetmatrix){
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
edges <- data.table(projectingKeywordNetwork$coterm[,c(1,2,4)])
binetmatrix <- binetMaxCompart
# patch for similarity
# simedge==0
d_ply(edges,.(id),function(edge_a,edges,binetmatrix){
  e <- edges[which(edges$id>edge_a$id & (edges$i == edge_a$i | edges$i == edge_a$j | edges$j == edge_a$i | edges$j == edge_a$j)),]
  print(paste(edge_a$id,nrow(e),sep = "-"))
  if(nrow(e)!=0){
    d_ply(e,.(id),function(edge_b,edge_a,binetmatrix){
      # similarity calculation of each
      simedge_old <- edgeSimilarity(edge_a,edge_b,binetmatrix)
      simedge <- edgeSimilarity_patch(edge_a,edge_b,binetmatrix)
      if(simedge_old==0&simedge!=0){
        #data.table(a_id=edge_a$id,b_id=edge_b$id,sim=simedge)
        write(c(edge_a$id,edge_b$id,simedge),"edgeSimilarity_patch",ncolumns = 3,append = T,sep = "\t")
      }
    },edge_a,binetmatrix,.progress = "text")
  }
},edges,binetmatrix,.progress = "text")


edgeCommunityTreeGeneration <- function(edges,similarity,rankNo=1){
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
    write.table(edges,file = "edgeCommunityTree_patch",quote = F,sep = "\t",row.names = F,col.names = T)
    print(paste(ncol(edges)-3,rankNo,nrow(edgepairs),"generation finished!",sep = "-"))
    rankNo <- rankNo + nrow(similarity[which(similarity$rank==rankNo),])
  }
  return(edges)
}
# continue generation
similarity <- read.table(file = "edgeSimilarity_patch",header = F,sep = "\t",col.names = c("a_id","b_id","sim"),stringsAsFactors = F)
# ranking similarity edge pair list (decrease)
similarity$rank <- length(similarity$sim) - rank(similarity$sim,ties.method = "max") + 1
similarity <- as.data.table(similarity)
# continue run
edgesTree <- read.table(file = "edgeCommunityTree",header = T,sep = "\t",stringsAsFactors = F)
edgesTree <- as.data.table(edgesTree)
nrow(edgesTree)-3
rankNo <- 1 #a new start
rankNo <- rankNo + nrow(similarity[which(similarity$rank==rankNo),])
edgesCommunityTree <- edgeCommunityTreeGeneration(edgesTree,similarity,rankNo)