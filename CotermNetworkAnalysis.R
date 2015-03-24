# network generation and simplify
library(igraph)
g_coterm <- graph.edgelist(el = as.matrix(projectingKeywordNetwork$coterm[,1:2]),directed = FALSE)
g_coterm$name  <- "co-term"
E(g_coterm)$weight <- projectingKeywordNetwork$coterm[,3]
V(g_coterm)$keyword <- projectingKeywordNetwork$keyword
V(g_coterm)$docfreq <- colSums(binetMaxCompart)
is.simple(g_coterm)
g_coterm <- simplify(g_coterm)
is.simple(g_coterm)
#analysis components
is.connected(g_coterm)
assortativity.degree(graph = g_coterm,directed = F)
# B:f(k1,k2)=(d(k1)*d(k2))^alpha
V(g_coterm)$degree <- degree(g_coterm)
projectingKeywordNetwork$cotermwithdegree <- merge(x = projectingKeywordNetwork$coterm,y = data.frame(id = 1:max(V(g_coterm)),jdegree = V(g_coterm)$degree),by.x = "j",by.y = "id",all.x = TRUE,sort = FALSE)
projectingKeywordNetwork$cotermwithdegree <- merge(x = projectingKeywordNetwork$cotermwithdegree,y = data.frame(id = 1:max(V(g_coterm)),idegree = V(g_coterm)$degree),by.x = "i",by.y = "id",all.x = TRUE,sort = FALSE)
projectingKeywordNetwork$cotermwithdegree <- merge(x = projectingKeywordNetwork$cotermwithdegree,y = data.frame(id = 1:max(V(g_coterm)),jdocfreq = V(g_coterm)$docfreq),by.x = "j",by.y = "id",all.x = TRUE,sort = FALSE)
projectingKeywordNetwork$cotermwithdegree <- merge(x = projectingKeywordNetwork$cotermwithdegree,y = data.frame(id = 1:max(V(g_coterm)),idocfreq = V(g_coterm)$docfreq),by.x = "i",by.y = "id",all.x = TRUE,sort = FALSE)
projectingKeywordNetwork$cotermwithdegree$percentw <- projectingKeywordNetwork$cotermwithdegree$w/sum(projectingKeywordNetwork$cotermwithdegree$w)
projectingKeywordNetwork$cotermwithdegree$percenti <- projectingKeywordNetwork$cotermwithdegree$idocfreq/sum(V(g_coterm)$docfreq)
projectingKeywordNetwork$cotermwithdegree$percentj <- projectingKeywordNetwork$cotermwithdegree$jdocfreq/sum(V(g_coterm)$docfreq)
gp <- ggplot(projectingKeywordNetwork$cotermwithdegree)
gp + geom_point(aes(x = percentw/percentj, y = percenti))
gp + geom_point(aes(x = w/idocfreq, y = w/jdocfreq))
gp + geom_point(aes(x = w/idocfreq, y = w))
gp + geom_point(aes(x = log(idegree*jdegree), y = log(w)))
gp + geom_point(aes(x = log((idegree*jdegree)^0.5/percentw), y = w))
gp + geom_point(aes(x = ((idegree*jdegree)^0.5/w)^0.5, y = w))
gp + geom_point(aes(x = log((idegree*jdegree)^0.5), y = log(w)))
library(rgl)
plot3d(x = projectingKeywordNetwork$cotermwithdegree$idegree,y = projectingKeywordNetwork$cotermwithdegree$jdegree,z = projectingKeywordNetwork$cotermwithdegree$w,type = "h")
plot3d(x = projectingKeywordNetwork$cotermwithdegree$idegree,y = projectingKeywordNetwork$cotermwithdegree$jdegree,z = projectingKeywordNetwork$cotermwithdegree$w/sum(projectingKeywordNetwork$cotermwithdegree$w))
plot3d(x = projectingKeywordNetwork$cotermwithdegree$idegree,y = projectingKeywordNetwork$cotermwithdegree$jdegree,z = log((projectingKeywordNetwork$cotermwithdegree$idegree*projectingKeywordNetwork$cotermwithdegree$jdegree)^0.5/projectingKeywordNetwork$cotermwithdegree$w))
plot3d(x = projectingKeywordNetwork$cotermwithdegree$w/projectingKeywordNetwork$cotermwithdegree$idocfreq,
       y = projectingKeywordNetwork$cotermwithdegree$w/projectingKeywordNetwork$cotermwithdegree$jdocfreq,
       z = projectingKeywordNetwork$cotermwithdegree$w,type = "h")
# clean useless object
addPersistentObjects("g_coterm")
rmTempObject()