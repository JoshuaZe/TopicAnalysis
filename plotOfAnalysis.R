library(dplyr)
library(ggplot2)
library(igraph)
library(wordcloud)
# 3-1
#png('3-1.png', height = 300, width = 800)
tmp <- paperMeta %>% count(publication_year)
tmp$publication_year <- as.integer(tmp$publication_year)
g <- ggplot(tmp,mapping = aes(publication_year,n))
g <- g + geom_point()
g <- g + labs(x='年份', y='论文数')
g
#dev.off()

g <- coterm %>% filter((keyword1=="model"|keyword2=="model")&nchar(keyword1)<12&nchar(keyword2)<12)
g
g_coterm <- graph.edgelist(el = as.matrix(g),directed = FALSE)
is.simple(g_coterm)
g_coterm <- simplify(g_coterm)
is.simple(g_coterm)
g <- g_coterm
E(g)
V(g)$keywords <- c("small firms","model","design","evaluation","performance","validation","adoption",
                   "internet","web","arima","forecasting","information","lead time","consistency",
                   "elicitation","modelling","metamodel"  )
ids <- NULL
for(k in V(g)$keywords){ids <- c(ids,which(colnames(binetMaxCompart) == k))}
V(g)$no <- ids
E(g)$no <- 1:16
E(g)$width <- 2
E(g)$color <- as.numeric(LinkTopicTree_Model[4,])+5
#simple drawing
par(mar = c(0, 0, 0, 0))
set.seed(23)
plot(g,layout=layout.graphopt,
     vertex.size = 5,vertex.label.cex=1,edge.label=E(g)$no,edge.label.color="red",
     edge.label.cex=1,edge.arrow.size=0.2,edge.color=E(g)$color,edge.width=2)
tkplot(g)
rglplot(g)
