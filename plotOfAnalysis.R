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
