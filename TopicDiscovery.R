# http://en.wikipedia.org/wiki/Topics_(Aristotle)#What_is_a_.22topic.22.3F
# http://en.wikipedia.org/wiki/Topic_Maps
#####
# 主题发现
# 1.how to define a topic？
# definition : 特定语境下构造论文实例的模板
# 不同的方式运用关键词刻画话题 - 概率与网络结构
# other definition one : a topic is a bag of keywords with probabilistic
# other definition two : a topic is a stable/Widely accepted structure of keywords
#####
#library(tm)
#paperKeywordBinDTM <- as.DocumentTermMatrix(paperKeywordMatrix,weightBin)
#####
# 二分图分隔子图分析预处理
#####
library(bipartite)
library(ggplot2)
library(dplyr)
binet <- paperKeywordMatrix
binetCompart <- compart(binet)
binetCompart$n.compart
paperCompart <- data.frame(paper=row.names(binetCompart$cweb),compart=-apply(binetCompart$cweb,1,FUN = min))
compartofeach <- paperCompart %>% group_by(compart) %>% summarise(cnt = n())
compartofeach$cnt
ggplot(compartofeach,aes(x = compart,y = cnt)) + geom_point()
binetMaxCompart <- paperKeywordMatrix[row.names(paperKeywordMatrix) %in% (paperCompart %>% filter(compart==2))[,1],]
binetMaxCompart[which(binetMaxCompart!=0)] <- 1
binetMaxCompart <- empty(binetMaxCompart)
# A:degree distribution analysis
# 结论:关键词是幂率分布,关键词的度存在长尾(少部分关键词被广泛使用,大多数关键词只在一两篇论文中使用)
# 根据优先连接的规则,度高的关键词更容易被论文使用
# 并且二分网络下度高的关键词其投影(共词网络)的关键词度也高
degreedistr(binetMaxCompart)
# clean useless object
addPersistentObjects("binetMaxCompart")
rmTempObject()
#moduleWebObject  <-  computeModules(binetMaxCompart)
#moduleList  <-  listModuleInformation(moduleWebObject)
#moduleCZ <- czvalues(moduleWebObject)
#plotModuleWeb(moduleWebObject)
#####
# coterm network
# B:共词网络/二分图话题发现技术 - 连边共词社团检测算法
# 做菜类比:每一位创作者从素材话题中选取特定关键词制作论文
# 连边共词社团检测算法 - 边袋模型
# 不同目的下话题划分方式不同
# 1.论文实例可以被类别领域清晰分割,而符合论文实例创作机制的话题也应当被该类别领域清晰分割
# 2.各个类别领域被非主要分配的惩罚不同（分类-杂志）
# 3.以上两点保证所选关键词是在同一个类别下,但不同话题关键词间距与同话题关键词间距远
#####
#http://toreopsahl.com/tnet/two-mode-networks/projection/
projectingKeywordNetwork <- list(keyword=colnames(binetMaxCompart),coterm=projecting_tm(t(binetMaxCompart),method = "sum"))
projectingKeywordNetwork$coterm$id  <- 1:nrow(projectingKeywordNetwork$coterm)
# clean useless object
addPersistentObjects("projectingKeywordNetwork")
rmTempObject()
# 连边共词社团检测算法
#library(foreach)
#library(doParallel)
library(plyr)
library(data.table)
#library(dplyr)
# core algorithms
edgeCommunityTreeGeneration <- function(edges,similarity,rankNo=1){
  # only one cluster left
  #if(length(unique(edges[,ncol(edges)]))==1) return(edges)
  if(nrow(unique(edges[,ncol(edges),with=F]))==1) return(edges)
  # generate one layer of tree
  edges$tmp <- edges[,ncol(edges),with=F]
  setnames(edges,"tmp",paste("cluster",ncol(edges)-3,sep = "_"))
  #colnames(edges)[ncol(edges)] <- paste("cluster",ncol(edges)-3,sep = "_")
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
#   for(i in 1:nrow(edgepairs)){
#     e_a <- edges[edgepairs$a_id,ncol(edges)]
#     e_b <- edges[edgepairs$b_id,ncol(edges)]
#     edges[which(edges[,ncol(edges)]==e_a),ncol(edges)] <- min(e_a,e_b)
#     edges[which(edges[,ncol(edges)]==e_b),ncol(edges)] <- min(e_a,e_b)
#   }
  # recursive
  write.table(edges,file = "edgeCommunityTree",quote = F,sep = "\t",row.names = F,col.names = T)
  print(paste(ncol(edges)-3,rankNo,nrow(edgepairs),"generation finished!",sep = "-"))
  rankNo <- rankNo + nrow(similarity[which(similarity$rank==rankNo),])
  return(edgeCommunityTreeGeneration(edges,similarity,rank))
}
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
    write.table(edges,file = "edgeCommunityTree",quote = F,sep = "\t",row.names = F,col.names = T)
    print(paste(ncol(edges)-3,rankNo,nrow(edgepairs),"generation finished!",sep = "-"))
    rankNo <- rankNo + nrow(similarity[which(similarity$rank==rankNo),])
  }
  return(edges)
}
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
    return(0)
  }
}
# clean useless object
addPersistentObjects("edgeCommunityTreeGeneration")
addPersistentObjects("edgeSimilarity")
rmTempObject()
save(file = "edgeCommunityDetection.RData",list = c("edgeCommunityTreeGeneration",
                                                    "edgeSimilarity",
                                                    "projectingKeywordNetwork",
                                                    "binetMaxCompart"))
# run
# edges : i j id
# binetmatrix : paper-keyword
edges <- data.table(projectingKeywordNetwork$coterm[,c(1,2,4)])
binetmatrix <- binetMaxCompart
#rm(binetMaxCompart)
#rm(projectingKeywordNetwork)
# calculate the similarity of each edge
d_ply(edges,.(id),function(edge_a,edges,binetmatrix){
  e <- edges[which(edges$id>edge_a$id & (edges$i == edge_a$i | edges$i == edge_a$j | edges$j == edge_a$i | edges$j == edge_a$j)),]
  print(paste(edge_a$id,nrow(e),sep = "-"))
  if(nrow(e)!=0){
    d_ply(e,.(id),function(edge_b,edge_a,binetmatrix){
      # similarity calculation of each
      simedge <- edgeSimilarity(edge_a,edge_b,binetmatrix)
      if(simedge!=0){
        #data.table(a_id=edge_a$id,b_id=edge_b$id,sim=simedge)
        write(c(edge_a$id,edge_b$id,simedge),"edgeSimilarity",ncolumns = 3,append = T,sep = "\t")
      }
    },edge_a,binetmatrix,.progress = "text")
  }
},edges,binetmatrix,.progress = "text")
#   similarity <- foreach(edge_a=iter(edges,by = 'row'),.combine=rbind) %do% {
#                   foreach(edge_b=iter(edges,by = 'row'),.combine=rbind) %do% {
#                     # similarity calculation of each
#                     simedge <- edgeSimilarity(edge_a,edge_b,binetmatrix)
#                     if((edge_a$id!=edge_b$id)&&simedge!=0&&simedge!=1)
#                       data.frame(a_id=edge_a$id,b_id=edge_b$id,sim=simedge)
#                   }
#                 }
similarity <- read.table(file = "edgeSimilarity",header = F,sep = "\t",col.names = c("a_id","b_id","sim"),stringsAsFactors = F)
# ranking similarity edge pair list (decrease)
similarity$rank <- length(similarity$sim) - rank(similarity$sim,ties.method = "max") + 1
similarity <- as.data.table(similarity)
# clean useless object
addPersistentObjects("similarity")
rmTempObject()
# save .RData
save(file = "VIP.RData",list = memoryWhiteList)
# edge community tree generation
# edges : i j id
# similarity : a_id b_id sim rank
edgesCommunityTree <- edgeCommunityTreeGeneration(edges,similarity)
edgesTree <- read.table(file = "edgeCommunityTree",header = T,sep = "\t",stringsAsFactors = F)
edgesTree <- as.data.table(edgesTree)
edgesTree <- edgesTree[,1:4,with=F]
rankNo <- 1 #Just Finished
rankNo <- rankNo + nrow(similarity[which(similarity$rank==rankNo),])
edgesCommunityTree <- edgeCommunityTreeGeneration(edgesTree,similarity,rankNo)
save(file = "edgeCommunityResult.RData",list = c("edgeCommunityDetection",
                                                    "edgeCommunityTreeGeneration",
                                                    "edgeSimilarity",
                                                    "edges",
                                                    "binetmatrix",
                                                    "similarity",
                                                    "edgesCommunityTree"))
# C:特定语境下话题划分评价

#####
# 主题存在性检测
# 话题存在性问题需要特定上下文/场景
# how can u make sure that u find a topic?
# do these so called topics exist?
# 特殊上下文语义背景下,话题存在的充分必要条件就是存在特定语境下的对应实例且可用于构造对应实例
# 1.可表达(人脑中有对应的概念就能构造),Topic -> Keywords
# 2.话题划分合理性度量(存在该语境下对应实例),Topics -> category
# 3.paper-topic二分图证明可以构造实例？
#####
# C:关键词表达提取/最大生成树/可表达性

###############################
# Drawing by wordcloud 
###############################
library(wordcloud)
par(mar=c(0,0,0,0))
pal <- brewer.pal(6,"Dark2")
png('ICIS2014Wordcloud.png', height = 300, width = 800)
wordcloud(words=attr_tag$keyword,freq=attr_tag$weight,scale=c(5,1),min.freq=10,max.words=500,
          random.order=F,random.color=F,rot.per=0,colors=pal,ordered.colors=F,
          use.r.layout=F,fixed.asp=F)
dev.off()
###############################
# draw graph of topic (community)
###############################
#ggplot drawing
library(sna)
#get one of the community
g<-delete.vertices(gg_coterm,gg_coterm.com$membership!=2)
tb <- as.matrix(get.adjacency(g))
m <- tb
plotcord <- data.frame(gplot.layout.kamadakawai(m,NULL))
row.names(plotcord) <- row.names(tb)
out <- NULL
for(i in 1:nrow(tb)){
  seg <- as.matrix(tb[i,tb[i,]>0])
  if(length(seg)!=0){
    seg <- cbind(seg,plotcord[i,]) 
    Xend <- plotcord[row.names(seg),]
    colnames(Xend) <- c("Xend1","Xend2")
    seg <- cbind(seg,Xend) 
    out <- rbind(out,seg)
  }
}
png('ICIS2014.png', height = 8000, width = 8000)
plt(out,plotcord)
dev.off()
###############################
# draw better network using ggplot
###############################
library(ggplot2)
plt<-function(out,plotcord){
  p <- ggplot(data=plotcord, aes(x=X1, y=X2, label=rownames(plotcord)))+geom_point(colour="steelblue",size=40)+geom_text(size=35,vjust=-1,colour="brown",font=3)+ geom_segment(aes(x=X1, y=X2, xend = Xend1, yend = Xend2), data=out[1,], size =out[1,1]*2 , colour="grey",alpha=0.01)+theme(panel.background = element_blank()) +theme(legend.position="none")+theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + theme( legend.background = element_rect(colour = NA)) + theme(panel.background = element_rect(fill = "white", colour = NA)) +theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  for(i in 2:nrow(out)){
    p<-p+geom_segment(aes(x=X1, y=X2, xend = Xend1, yend = Xend2), data=out[i,], size =out[i,1]*12 , colour="grey",alpha=0.01)}
  return(p)
}
