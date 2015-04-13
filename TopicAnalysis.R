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
library(igraph)
gg_coterm <- graph.edgelist(el = data[,1:2]),directed = FALSE)
E(gg_coterm)$weight <- data[,3]
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
png('network.png', height = 8000, width = 8000)
plt(out,plotcord)
dev.off()
###############################
# draw better network using ggplot
###############################
library(ggplot2)
plt<-function(out,plotcord){
  p <- ggplot(data=plotcord, aes(x=X1, y=X2, label=rownames(plotcord)))+geom_point(colour="steelblue",size=40)+geom_text(size=35,vjust=-1,colour="brown",font=3)+ geom_segment(aes(x=X1, y=X2, xend = Xend1, yend = Xend2), data=out[1,], size =out[1,1]*2 , colour="grey",alpha=0.01)+theme(panel.background = element_blank()) +theme(legend.position="none")+theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + theme( legend.background = element_rect(colour = NA)) + theme(panel.background = element_rect(fill = "white", colour = NA)) +theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  for(i in 2:nrow(out)){
    p<-p+geom_segment(aes(x=X1, y=X2, xend = Xend1, yend = Xend2), data=out[i,], size =out[i,]*12 , colour="grey",alpha=0.01)}
  return(p)
}
