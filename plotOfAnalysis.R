library(dplyr)
library(ggplot2)
# 3-1
#png('3-1.png', height = 300, width = 800)
tmp <- paperMeta %>% count(publication_year)
tmp$publication_year <- as.integer(tmp$publication_year)
g <- ggplot(tmp,mapping = aes(publication_year,n))
g <- g + geom_point()
g <- g + labs(x='年份', y='论文数')
g
#dev.off()

g <- coterm %>% filter((keyword1=="model"|keyword2=="model")&nchar(keyword1)<14&nchar(keyword2)<14)
g
library(igraph)
g_coterm <- graph.edgelist(el = as.matrix(g),directed = FALSE)
is.simple(g_coterm)
g_coterm <- simplify(g_coterm)
is.simple(g_coterm)
g <- g_coterm
#simple drawing
par(mar = c(0, 0, 0, 0))
set.seed(23)
plot(g,layout=layout.graphopt,
     vertex.size = 3,vertex.label.cex=1,
     edge.label.cex=1,edge.arrow.size=0.2)
###############################
# draw graph of topic (community)
###############################
#ggplot drawing
library(sna)
#get one of the community
#g<-delete.vertices(gg_coterm,gg_coterm.com$membership!=2)
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
png('modelExample.png', height = 1000, width = 1000)
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