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
which.max(PartitionDensity$V2<=414)
PartitionDensity$V2[5888]
PartitionDensity$V2[81075]
PartitionDensity$V2[66483]
PartitionDensity$V2[58672] #961
PartitionDensity$V2[54785] #999
PartitionDensity$V2[45000] #1269
PartitionDensity$V2[39491] #1500
PartitionDensity$V2[31351] #1936
PartitionDensity$V2[30325] #2000
PartitionDensity$V2[30000] #2034
PartitionDensity$V2[25942] #2500 ok
PartitionDensity$V2[22801] #2500 ok
PartitionDensity$V2[15483] #5000 ok
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

# similarityMST
simMST <- data.frame(V1=sort(unique(as.numeric(similarityMST$V3)),decreasing = T),id = 1:length(unique(as.numeric(similarityMST$V3))))
png('simMST.png', height = 600, width = 800)
g <- ggplot()
g <- g + geom_point(simMST,mapping = aes(id,V1))
g <- g + geom_line(simMST,mapping = aes(id,V1))
g <- g + geom_line(simMST,mapping = aes(5888,V1))
g <- g + geom_text(simMST,mapping = aes(5888,1,label = "=>5888次合并",vjust = 0,hjust = 0))
g <- g + geom_line(simMST,mapping = aes(31351,V1))
g <- g + geom_text(simMST,mapping = aes(31351,3,label = "31351次合并<=",vjust = 0,hjust = 1))
g <- g + labs(x='合并次数', y='合并连边相似度',colour = "")
g
dev.off()
CombinationInfluence$V1 <- as.numeric(CombinationInfluence$V1)
CombinationInfluence$V2 <- 1:nrow(CombinationInfluence)
CombinationInfluence$V3 <- cumsum(CombinationInfluence$V1)
which.max(CombinationInfluence$V3>=10)
png('cumCI.png', height = 600, width = 800)
g <- ggplot()
g <- g + geom_point(CombinationInfluence[1:nrow(CombinationInfluence),],mapping = aes(V2,V3))
g <- g + geom_line(CombinationInfluence[1:nrow(CombinationInfluence),],mapping = aes(V2,10))
g <- g + geom_text(CombinationInfluence[1:nrow(CombinationInfluence),],mapping = aes(60000,9.5,label = "累计话题合并影响力上限10",vjust = 1,hjust = 0))
g <- g + geom_line(CombinationInfluence[1:nrow(CombinationInfluence),],mapping = aes(5888,V3))
g <- g + geom_text(CombinationInfluence[1:nrow(CombinationInfluence),],mapping = aes(5888,1,label = "=>5888次合并",vjust = 0,hjust = 0))
g <- g + geom_line(CombinationInfluence[1:nrow(CombinationInfluence),],mapping = aes(31351,V3))
g <- g + geom_text(CombinationInfluence[1:nrow(CombinationInfluence),],mapping = aes(31351,6,label = "31351次合并<=",vjust = 0,hjust = 1))
g <- g + labs(x='合并次数', y='累计话题合并影响力',colour = "")
g
dev.off()
# input topic
g_Topic1
g <- graph.edgelist(el = as.matrix(g_Topic1),directed = FALSE)
#simple drawing
par(mar = c(0, 0, 0, 0))
set.seed(12)
plot(g,layout=layout.graphopt,
     vertex.size = 5,vertex.label.cex=1,edge.label.color="red",
     edge.label.cex=1,edge.arrow.size=0.2,edge.width=2)
tkplot(g)
rglplot(g)

# input topic keywords
cloud_RepresentationDegree
cloud <- cloud_RepresentationDegree
cloud$V2 <- as.integer(cloud_RepresentationDegree$V2 * 100000) + 2
cloud
par(mar = c(0, 0, 0, 0))
pal <- brewer.pal(6,"Dark2")
wordcloud(words=cloud$V1,freq=cloud$V2,scale=c(2,0.4),min.freq=1,max.words=500,
          random.order=F,random.color=F,rot.per=0,colors=pal,ordered.colors=F,
          use.r.layout=F,fixed.asp=F)
#LDA
library(topicmodels)
#library(lda)
k <- 500
SEED <- 2016
VSM_dtm <- binetMaxCompart
result.Gibbs500 <- LDA(VSM_dtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
result.Gibbs100 <- LDA(VSM_dtm, k = 100, method = "Gibbs",
                    control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
result.Gibbs200 <- LDA(VSM_dtm, k = 200, method = "Gibbs",
                       control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
result.Gibbs50 <- LDA(VSM_dtm, k = 50, method = "Gibbs",
                       control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
result.Gibbs415 <- LDA(VSM_dtm, k = 415, method = "Gibbs",
                      control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
result.Gibbs1936 <- LDA(VSM_dtm, k = 1936, method = "Gibbs",
                       control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))

perplexity(result.Gibbs,VSM_dtm)
posterior(result.Gibbs)$terms
posterior(result.Gibbs)$topic
topics(result.Gibbs,1)
terms(result.Gibbs)
result.Gibbs <- result.Gibbs1936
# model assessment
# data label
conn <- dbConnect(MySQL(), dbname = "yuli")
dbListTables(conn)
dbListFields(conn, "paper_label")
paperLabel <- dbReadTable(conn, "paper_label")
Magazine_Field <- table(paperLabel$full_source_title,paperLabel$subject_category)
#####
# result PTM - LDA theta
LDA_theta <- cbind(as.data.frame(posterior(result.Gibbs)$topic),id = rownames(posterior(result.Gibbs)$topic),stringsAsFactors = FALSE)
n <- ncol(LDA_theta) - 1
Result_LDA <- inner_join(LDA_theta,paperLabel,by = c("id"="item_ut"))
### method two : theta_SC_Topic -> heat map
sample_distribution <- Result_LDA[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise(n(),n()/length(unique(Result_LDA$id)))
theta_SC_LDATopic <- data.frame(Result_LDA[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise_each(funs(mean)))
SC_LDATopic_TF <- apply(X = theta_SC_LDATopic[,2:(n+1)],MARGIN = 2,FUN = function(x){x==max(x)})
# heat map
x <- as.matrix(theta_SC_LDATopic[,2:(n+1)])
x_TF <- SC_LDATopic_TF
theta_SC_LDATopic$subject_category
row.names(x)<-c("BIZ","CS-AI","CS-IS","CS-SE","IS&LS","MGMT","OR&MS")
row.names(x)<-theta_SC_LDATopic$subject_category
heatmap(x, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE)
x_fin <- as.matrix(x*x_TF)
row.names(x_fin)<-c("BIZ","CS-AI","CS-IS","CS-SE","IS&LS","MGMT","OR&MS")
row.names(x_fin)<-theta_SC_LDATopic$subject_category
#x_fin <- x_fin[c(4,1,2,6,3,7,5),]
heatmap(x_fin,Rowv = NA, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE)
# calculate Confusion Matrix
sensitivity <- recall <- apply(x_fin,1,sum)/apply(x,1,sum)
miss_rate <- 1 - recall
recall
precision <- apply(x_fin,1,sum)/apply(matrix(apply(x,2,sum),nrow = nrow(x),ncol = ncol(x),byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)
precision
distribution <- apply(x,1,sum)/sum(apply(x,1,sum))
detection_prevalence <- apply(matrix(apply(x,2,sum),nrow = nrow(x),ncol = ncol(x),byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)/sum(apply(x,1,sum))
detection_prevalence
detection_rate <- detection_prevalence * precision
detection_rate
accuracy <- sum(detection_rate)
error_rate <- 1 - accuracy
accuracy
f_beta <- 1
f1_score <- f_measure <- (f_beta ^ 2 + 1) * precision * recall / (f_beta ^ 2 * precision + recall)
f1_score
mean(f1_score)

#####
# result Our New NTM - LTM theta 1500
dim(LTM_theta)
n <- ncol(LTM_theta)
dim(papers_filter_idlist)
colnames(papers_filter_idlist) <- "id"
LTM_theta <- cbind(LTM_theta,id = papers_filter_idlist,stringsAsFactors = FALSE)
Result_LTM <- inner_join(LTM_theta,paperLabel,by = c("id"="item_ut"))
# method two : theta_SC_Topic -> heat map
sample_distribution <- Result_LTM[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise(n(),n()/length(unique(Result_LTM$id)))
sample_distribution
theta_SC_LTMTopic <- data.frame(Result_LTM[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise_each(funs(mean)))
SC_LTMTopic_TF <- apply(X = theta_SC_LTMTopic[,2:(n+1)],MARGIN = 2,FUN = function(x){x==max(x)})
# heat map
x <- as.matrix(theta_SC_LTMTopic[,2:(n+1)])
x_TF <- SC_LTMTopic_TF
#start analysis
row.names(x)<-c("BIZ","CS-AI","CS-IS","CS-SE","IS&LS","MGMT","OR&MS")
row.names(x)<-theta_SC_LTMTopic$subject_category
heatmap(x, scale = "column",col=cm.colors(256),xlab = "Topics",revC=T,distfun = function(x) dist(x,method = "minkowski",p = 2))
x_fin <- as.matrix(x*x_TF)
row.names(x_fin)
#x_fin <- x_fin[c(4,1,2,6,3,7,5),]
#x_fin <- x_fin[c(4,1,2,6,3,7,5),]heatmap(x_fin,Rowv = NA, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE,distfun = function(x) dist(x,method = "minkowski",p = 1))
# calculate Confusion Matrix
sensitivity <- recall <- apply(x_fin,1,sum)/apply(x,1,sum)
miss_rate <- 1 - recall
recall
precision <- apply(x_fin,1,sum)/apply(matrix(apply(x,2,sum),nrow = nrow(x),ncol = ncol(x),byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)
precision
distribution <- apply(x,1,sum)/sum(apply(x,1,sum))
detection_prevalence <- apply(matrix(apply(x,2,sum),nrow = nrow(x),ncol = ncol(x),byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)/sum(apply(x,1,sum))
detection_prevalence
detection_rate <- detection_prevalence * precision
detection_rate
accuracy <- sum(detection_rate)
error_rate <- 1 - accuracy
accuracy
f_beta <- 1
f1_score <- f_measure <- (f_beta ^ 2 + 1) * precision * recall / (f_beta ^ 2 * precision + recall)
f1_score
mean(f1_score)

#####
# result Traditional NTM - TNTM theta
dim(TNTM_m)
dim(papers_idlist)
n <- ncol(TNTM_m)
colnames(papers_idlist) <- "id"
TNTM_theta <- cbind(TNTM_m/apply(TNTM_m,1,sum),id = papers_idlist,stringsAsFactors = FALSE)
Result_TNTM <- inner_join(TNTM_theta,paperLabel,by = c("id"="item_ut"))
# method two : theta_SC_Topic -> heat map
sample_distribution <- Result_TNTM[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise(n(),n()/length(unique(Result_TNTM$id)))
theta_SC_TNTMTopic <- data.frame(Result_TNTM[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise_each(funs(mean)))
SC_TNTMTopic_TF <- apply(X = theta_SC_TNTMTopic[,2:(n+1)],MARGIN = 2,FUN = function(x){x==max(x)})
# heat map
x <- as.matrix(theta_SC_TNTMTopic[,2:(n+1)])
x_TF <- SC_TNTMTopic_TF
row.names(x)<-c("BIZ","CS-AI","CS-IS","CS-SE","IS&LS","MGMT","OR&MS")
row.names(x)<-theta_SC_TNTMTopic$subject_category
heatmap(x, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE,distfun = function(x) dist(x,method = "euclidean"))
x_fin <- as.matrix(x*x_TF)
#x_fin <- x_fin[c(4,1,2,7,6,3,5),]
heatmap(x_fin,Rowv = NA, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE,distfun = function(x) dist(x,method = "minkowski",p=1))
# calculate Confusion Matrix
sensitivity <- recall <- apply(x_fin,1,sum)/apply(x,1,sum)
miss_rate <- 1 - recall
recall
precision <- apply(x_fin,1,sum)/apply(matrix(apply(x,2,sum),nrow = nrow(x),ncol = ncol(x),byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)
precision
distribution <- apply(x,1,sum)/sum(apply(x,1,sum))
detection_prevalence <- apply(matrix(apply(x,2,sum),nrow = nrow(x),ncol = ncol(x),byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)/sum(apply(x,1,sum))
detection_prevalence
detection_rate <- detection_prevalence * precision
detection_rate
accuracy <- sum(detection_rate)
error_rate <- 1 - accuracy
accuracy
f_beta <- 1
f1_score <- f_measure <- (f_beta ^ 2 + 1) * precision * recall / (f_beta ^ 2 * precision + recall)
f1_score
mean(f1_score)



#####
n <- 1936
theta_M <- matrix(data = round(runif(8944*n,min=0,max=1)),nrow = 8944,ncol = n)
# result Our New NTM - average theta
dim(theta_M)
dim(papers_idlist)
n <- ncol(theta_M)
colnames(papers_idlist) <- "id"
theta <- cbind(theta_M/apply(theta_M,1,sum),id = papers_idlist,stringsAsFactors = FALSE)
Result_theta <- inner_join(theta,paperLabel,by = c("id"="item_ut"))
# method two : theta_SC_Topic -> heat map
sample_distribution <- Result_theta[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise(n(),n()/length(unique(Result_theta$id)))
theta_SC_Topic <- data.frame(Result_theta[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise_each(funs(mean)))
SC_Topic_TF <- apply(X = theta_SC_Topic[,2:(n+1)],MARGIN = 2,FUN = function(x){x==max(x)})
# heat map
x <- as.matrix(theta_SC_Topic[,2:(n+1)])
x_TF <- SC_Topic_TF
#row.names(x)<-c("BIZ","CS-AI","CS-IS","CS-SE","IS&LS","MGMT","OR&MS")
row.names(x)<-theta_SC_Topic$subject_category
#heatmap(x, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE,distfun = function(x) dist(x,method = "euclidean"))
x_fin <- as.matrix(x*x_TF)
#x_fin <- x_fin[c(4,1,2,7,6,3,5),]
#heatmap(x_fin,Rowv = NA, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE,distfun = function(x) dist(x,method = "minkowski",p=1))
# calculate Confusion Matrix
sensitivity <- recall <- apply(x_fin,1,sum)/apply(x,1,sum)
miss_rate <- 1 - recall
recall
precision <- apply(x_fin,1,sum)/apply(matrix(apply(x,2,sum),nrow = nrow(x),ncol = ncol(x),byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)
precision
distribution <- apply(x,1,sum)/sum(apply(x,1,sum))
detection_prevalence <- apply(matrix(apply(x,2,sum),nrow = nrow(x),ncol = ncol(x),byrow = T,dimnames = list(rownames(x),colnames(x)))*x_TF,1,sum)/sum(apply(x,1,sum))
detection_prevalence
detection_rate <- detection_prevalence * precision
detection_rate
accuracy <- sum(detection_rate)
error_rate <- 1 - accuracy
accuracy
f_beta <- 1
f1_score <- f_measure <- (f_beta ^ 2 + 1) * precision * recall / (f_beta ^ 2 * precision + recall)
f1_score
mean(f1_score)

#####
# magazine analysis
#####
# result Our New NTM - LTM theta
dim(LTM_theta_orginal)
n <- ncol(LTM_theta_orginal)
dim(papers_filter_idlist)
colnames(papers_filter_idlist) <- "id"
LTM_theta <- cbind(LTM_theta_orginal,id = papers_filter_idlist,stringsAsFactors = FALSE)
Result_LTM <- inner_join(LTM_theta,paperLabel,by = c("id"="item_ut"))
# method two : theta_SC_Topic -> heat map
Result_LTM[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise(n(),n()/length(unique(Result_LTM$id)))
Result_LTM[,c(2:(n+1),(n+4))] %>% group_by(full_source_title) %>% summarise(n(),n()/length(unique(Result_LTM$id)))
Result_LTM[,c(2:(n+1),(n+3))] %>% group_by(publication_year) %>% summarise(n(),n()/length(unique(Result_LTM$id)))
theta_Magazine_Topic <- data.frame(Result_LTM[,c(2:(n+1),(n+4))] %>% group_by(full_source_title) %>% summarise_each(funs(mean)))
Magazine_Topic_TF <- apply(X = theta_Magazine_Topic[,2:(n+1)],MARGIN = 2,FUN = function(x){x==max(x)})
# heat map
x <- as.matrix(theta_Magazine_Topic[,2:(n+1)])
x_TF <- Magazine_Topic_TF
#start analysis
row.names(x)<-theta_Magazine_Topic$full_source_title
heatmap(x, scale = "column",col=cm.colors(256),xlab = "Topics",revC=T,distfun = function(x) dist(x,method = "minkowski",p = 2))
x_fin <- as.matrix(x*x_TF)
row.names(x_fin)
#x_fin <- x_fin[c(4,1,2,6,3,7,5),]
heatmap(x_fin,Rowv = NA, scale = "column",col=cm.colors(256),xlab = "Topics",revC=TRUE,distfun = function(x) dist(x,method = "minkowski",p = 1))
# Top Topic
x_fin[1,which(x_fin[1,]>0.015)]
x_fin[2,which(x_fin[2,]>0.0037)]
x_fin[3,which(x_fin[3,]>0.0099)]
x_fin[4,which(x_fin[4,]>0.00318)]
x_fin[5,which(x_fin[5,]>0.00338)]
x_fin[6,which(x_fin[6,]>0.0051)]
x_fin[7,which(x_fin[7,]>0.0136)]
x_fin[8,which(x_fin[8,]>0.00492)]
x_fin[9,which(x_fin[9,]>0.0037)]
x_fin[10,which(x_fin[10,]>0.005)]

x[1,which(x[1,]>0.015)]
x[2,which(x[2,]>0.0037)]
x[3,which(x[3,]>0.0099)]
x[4,which(x[4,]>0.00318)]
x[5,which(x[5,]>0.00338)]
x[6,which(x[6,]>0.0051)]
x[7,which(x[7,]>0.0136)]
x[8,which(x[8,]>0.00492)]
x[9,which(x[9,]>0.0037)]
x[10,which(x[10,]>0.005)]
#####
# Trend analysis
#####
# result Our New NTM - LTM theta
dim(LTM_theta_orginal)
n <- ncol(LTM_theta_orginal)
dim(papers_filter_idlist)
colnames(papers_filter_idlist) <- "id"
LTM_theta <- cbind(LTM_theta_orginal,id = papers_filter_idlist,stringsAsFactors = FALSE)
Result_LTM <- inner_join(LTM_theta,paperLabel,by = c("id"="item_ut"))
# method two : theta_SC_Topic -> heat map
Result_LTM[,c(2:(n+1),(n+5))] %>% group_by(subject_category) %>% summarise(n(),n()/length(unique(Result_LTM$id)))
Result_LTM[,c(2:(n+1),(n+4))] %>% group_by(full_source_title) %>% summarise(n(),n()/length(unique(Result_LTM$id)))
Result_LTM[,c(2:(n+1),(n+3))] %>% group_by(publication_year) %>% summarise(n(),n()/length(unique(Result_LTM$id)))
theta_Year_Topic <- data.frame(Result_LTM[,c(2:(n+1),(n+3))] %>% group_by(publication_year) %>% summarise_each(funs(mean)))
#reshape
theta_year_topic_long <- reshape(theta_Year_Topic, idvar = "publication_year", varying = list(2:(n+1)),
                                 v.names = "hotdegree", direction = "long")
theta_year_topic_long$publication_year <- as.integer(theta_year_topic_long$publication_year)
theta_year_topic_long$time <- as.character(theta_year_topic_long$time)
colnames(theta_year_topic_long) <- c("publication_year","Topic","hotdegree")
#Trend
which(apply(theta_Year_Topic[,2:(n+1)],2,mean)>0.003)
png('Trend.png', height = 800, width = 1000)
g <- ggplot(theta_year_topic_long[which(theta_year_topic_long$Topic %in% c(10,22,28,352)),],aes(x = publication_year,y = hotdegree,colour = Topic))
g <- g + geom_line()
g <- g + labs(x='年份', y='热度')
g
dev.off()
png('Trend.png', height = 800, width = 1000)
g <- ggplot(theta_year_topic_long[which(theta_year_topic_long$Topic %in% c(321)),],aes(x = publication_year,y = hotdegree,colour = Topic))
g <- g + geom_line()
g <- g + labs(x='年份', y='热度')
g
dev.off()
png('Trend1.png', height = 300, width = 400)
g <- ggplot(theta_year_topic_long[which(theta_year_topic_long$Topic %in% c(36,81)),],aes(x = publication_year,y = hotdegree,colour = Topic))
g <- g + geom_line(size = 1)
g <- g + labs(x='年份', y='热度')
g
dev.off()
png('Trend2.png', height = 300, width = 400)
g <- ggplot(theta_year_topic_long[which(theta_year_topic_long$Topic %in% c(64,321)),],aes(x = publication_year,y = hotdegree,colour = Topic))
g <- g + geom_line(size = 1)
g <- g + labs(x='年份', y='热度')
g
dev.off()
png('Trend3.png', height = 300, width = 400)
g <- ggplot(theta_year_topic_long[which(theta_year_topic_long$Topic %in% c(352)),],aes(x = publication_year,y = hotdegree,colour = Topic))
g <- g + geom_line(size = 1)
g <- g + labs(x='年份', y='热度')
g
dev.off()
png('Trend4.png', height = 300, width = 400)
g <- ggplot(theta_year_topic_long[which(theta_year_topic_long$Topic %in% c(1)),],aes(x = publication_year,y = hotdegree,colour = Topic))
g <- g + geom_line(size = 1)
g <- g + labs(x='年份', y='热度')
g
dev.off()
# field
Year_Field <- table(paperLabel$publication_year,paperLabel$subject_category)
Year_Field_P <- as.data.frame(Year_Field/apply(Year_Field,1,sum),stringsAsFactors = F)
colnames(Year_Field_P) <- c("publication_year","Field","hotdegree")
Year_Field_P$publication_year <- as.integer(Year_Field_P$publication_year)
png('Trend_part.png', height = 600, width = 800)
g <- ggplot(Year_Field_P[Year_Field_P$publication_year>=1970,],aes(x = publication_year,y = hotdegree,colour = Field))
g <- g + geom_line(size=1)
g <- g + labs(x='年份', y='热度')
g
dev.off()
