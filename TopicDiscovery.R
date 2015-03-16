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
# A:二分图分隔子图分析预处理
binet <- paperKeywordMatrix
binetCompart <- compart(binet)
ND(binet, normalised=TRUE)
# B:共词网络/二分图话题发现技术
# B:特定语境下话题划分评价

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
