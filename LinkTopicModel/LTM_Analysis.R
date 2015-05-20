setwd("F:/Github/TopicAnalysis/LinkTopicModel")
library(RMySQL)
library(dplyr)
library(ggplot2)
library(igraph)
library(wordcloud)
# partition Density
PartitionDensity$V1 <- as.numeric(PartitionDensity$V1)
PartitionDensity$V2 <- as.numeric(PartitionDensity$V2)
PartitionDensity$V3 <- 1:nrow(PartitionDensity)
maxid <- max(which(PartitionDensity$V1==max(PartitionDensity$V1)))
PartitionDensity$V2[maxid]
which.max(PartitionDensity$V2<=797)
PartitionDensity$V2[81075]
PartitionDensity$V2[66483]
png('4-3.png', height = 800, width = 1000)
g <- ggplot()
g <- g + geom_point(PartitionDensity,mapping = aes(V2,V1,colour="CoNeighborBased LTM"))
g <- g + geom_line(PartitionDensity,mapping = aes(V2,V1,colour="CoNeighborBased LTM"))
g <- g + geom_line(PartitionDensity,mapping = aes(V2[maxid],V1,colour="CoNeighborBased LTM"))
g <- g + geom_text(PartitionDensity,mapping = aes(V2[maxid],V1[maxid],label = paste("密度最优划分","(",V2[maxid],",",V1[maxid],")",sep = ""),vjust = 0,hjust = 0))
g <- g + geom_line(PartitionDensity2,mapping = aes(V2,V1,colour="InsideLinkBased LTM - 0.20 0.05"))
g <- g + geom_line(PartitionDensity4,mapping = aes(V2,V1,colour="InsideLinkBased LTM - 0.35 0.05"))
g <- g + geom_line(PartitionDensity5,mapping = aes(V2,V1,colour="InsideLinkBased LTM - 0.55 0.05"))
g <- g + geom_line(PartitionDensity3,mapping = aes(V2,V1,colour="InsideLinkBased LTM - 0.75 0.05"))
g <- g + geom_line(PartitionDensity6,mapping = aes(V2,V1,colour="InsideLinkBased LTM - 0.55 0.15"))
g <- g + labs(x='社区划分数量', y='平均划分密度',colour = "")
g
dev.off()
PartitionDensity6$V1 <- as.numeric(PartitionDensity6$V1)
PartitionDensity6$V2 <- as.numeric(PartitionDensity6$V2)
PartitionDensity6$V3 <- 1:nrow(PartitionDensity6)

maxid2 <- max(which(PartitionDensity2$V1==max(PartitionDensity2$V1)))

png('4-4.png', height = 800, width = 1000)
g <- ggplot()
g <- g + geom_line(PartitionDensity[1:7500,],mapping = aes(V3,V1,colour="CoNeighborBased LTM"))
g <- g + geom_line(PartitionDensity[1:7500,],mapping = aes(V3[maxid],V1,fill="black"))
g <- g + geom_text(PartitionDensity,mapping = aes(V3[maxid],0,label = V3[maxid],vjust = 0,hjust = 0))
g <- g + geom_line(PartitionDensity2,mapping = aes(V3,V1,colour="InsideLinkBased LTM - 0.20 0.05"))
g <- g + geom_line(PartitionDensity2,mapping = aes(V3[maxid2],V1,fill="black"))
g <- g + geom_text(PartitionDensity2,mapping = aes(V3[maxid2],0,label = V3[maxid2],vjust = 0,hjust = 0))
g <- g + geom_line(PartitionDensity4,mapping = aes(V3,V1,colour="InsideLinkBased LTM - 0.35 0.05"))
g <- g + geom_line(PartitionDensity5,mapping = aes(V3,V1,colour="InsideLinkBased LTM - 0.55 0.05"))
g <- g + geom_line(PartitionDensity3,mapping = aes(V3,V1,colour="InsideLinkBased LTM - 0.75 0.05"))
g <- g + geom_line(PartitionDensity6,mapping = aes(V3,V1,colour="InsideLinkBased LTM - 0.55 0.15"))
g <- g + labs(x='合并次数', y='平均划分密度',colour = "")
g
dev.off()

# input topic
g_Topic166
g <- graph.edgelist(el = as.matrix(g_Topic166),directed = FALSE)
#simple drawing
par(mar = c(0, 0, 0, 0))
set.seed(12)
plot(g,layout=layout.graphopt,
     vertex.size = 5,vertex.label.cex=1,edge.label.color="red",
     edge.label.cex=1,edge.arrow.size=0.2,edge.width=2)
tkplot(g)
rglplot(g)

# input topic keywords
cloud_RepresentationDegree166
cloud <- cloud_RepresentationDegree166
cloud$V2 <- as.integer(cloud_RepresentationDegree166$V2 * 1000) + 2
cloud
par(mar = c(0, 0, 0, 0))
pal <- brewer.pal(6,"Dark2")
wordcloud(words=cloud$V1,freq=cloud$V2,scale=c(1.6,1),min.freq=1,max.words=500,
          random.order=F,random.color=F,rot.per=0,colors=pal,ordered.colors=F,
          use.r.layout=F,fixed.asp=F)
#LDA
library(topicmodels)
#library(lda)
k <- 500
SEED <- 2016
VSM_dtm <- binetMaxCompart
result.Gibbs <- LDA(VSM_dtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
posterior(result.Gibbs)$terms
posterior(result.Gibbs)$topic
topics(result.Gibbs,1)
terms(result.Gibbs)

# model assessment
# data label
conn <- dbConnect(MySQL(), dbname = "yuli")
dbListTables(conn)
dbListFields(conn, "paper_label")
paperLabel <- dbReadTable(conn, "paper_label")
SC_Topic <- table(paperLabel$item_ut,paperLabel$subject_category)
#####
# result PTM - LDA theta
LDA_theta <- cbind(as.data.frame(posterior(result.Gibbs)$topic),id = rownames(posterior(result.Gibbs)$topic),stringsAsFactors = FALSE)
Result_LDA <- inner_join(LDA_theta,paperLabel,by = c("id"="item_ut"))
# method one : K-NN Classification using Topic Features -> Class <-> NO
# method two : theta_SC_Topic -> heat map
# method three : Paper-Top One Topic-Class <-> NO
sample_distribution <- Result_LDA[,c(2:501,505)] %>% group_by(subject_category) %>% summarise(n(),n()/length(unique(Result_LDA$id)))
theta_SC_LDATopic <- data.frame(Result_LDA[,c(2:501,505)] %>% group_by(subject_category) %>% summarise_each(funs(mean)))
SC_LDATopic_TF <- apply(X = theta_SC_LDATopic[,2:501],MARGIN = 2,FUN = function(x){x==max(x)})
# heat map
x <- as.matrix(theta_SC_LDATopic[,2:501])
x_TF <- SC_LDATopic_TF
theta_SC_LDATopic$subject_category
row.names(x)<-c("BIZ","CS-AI","CS-IS","CS-SE","IS&LS","MGMT","OR&MS")
heatmap(x, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE)
x_fin <- as.matrix(x*x_TF)
row.names(x_fin)<-c("BIZ","CS-AI","CS-IS","CS-SE","IS&LS","MGMT","OR&MS")
heatmap(x_fin, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE)
# calculate Confusion Matrix
sensitivity <- recall <- apply(x_fin,1,sum)/apply(x,1,sum)
miss_rate <- 1 - recall
recall
precision <- apply(x_fin,1,sum)/apply(matrix(apply(x,2,sum),nrow = 7,ncol = 500,byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)
precision
distribution <- apply(x,1,sum)/sum(apply(x,1,sum))
detection_prevalence <- apply(matrix(apply(x,2,sum),nrow = 7,ncol = 500,byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)/sum(apply(x,1,sum))
detection_prevalence
detection_rate <- detection_prevalence * precision
detection_rate
accuracy <- sum(detection_rate)
error_rate <- 1 - accuracy
accuracy
f_beta <- 1
f1_score <- f_measure <- (f_beta ^ 2 + 1) * precision * recall / (f_beta ^ 2 * precision + recall)
f1_score
#####
# result Our New NTM - LTM theta 500

#####
# compare of LDA and LTM

#####
# result Traditional NTM - TNTM theta

#####
# result Our New NTM - LTM theta 797

