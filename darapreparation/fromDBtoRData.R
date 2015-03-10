setwd("F:\Github\TopicAnalysis")
source(file = "util.R",encoding = "UTF-8")
Sys.setlocale(,"CHS")
# Experiment of the Research Topic Detection
# March 10, 2015
###############################
# odbcConnect
###############################
library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "yuli")
dbListTables(conn)
# meta data of paper
dbListFields(conn, "issue")
dbListFields(conn, "paper_issue")
dbListFields(conn, "paper_text")
res <- dbSendQuery(conn, "SELECT paper_issue.item_ut,publication_type,publication_year,publication_date,article_title,document_type,abstract FROM 
                          (paper_issue inner join issue on paper_issue.item_ui = issue.ui) inner join paper_text 
                          on paper_issue.item_ut = paper_text.item_ut")
paperMeta <- dbFetch(res,n = -1)
dbClearResult(res)
# citation evolution year of paper
dbListFields(conn, "paper_citation")
res <- dbSendQuery(conn, "SELECT item_UT as item_ut, R9 as item_t9, cited_ref_year FROM paper_citation")
paperCitationYear <- dbFetch(res,n = -1)
paperYearCitedMatrix <- table(paperCitationYear$item_ut,paperCitationYear$cited_ref_year)
dbClearResult(res)
# paper-author
dbListFields(conn, "paper_author_dais")
res <- dbSendQuery(conn, "SELECT ut as item_ut, author_rank, DAIS FROM paper_author_dais")
paperAuthor <- dbFetch(res,n = -1)
dbClearResult(res)
# paper-keywords
dbListFields(conn, "paper_author_keyword")
nrow(dbReadTable(conn, "paper_author_keyword"))
res <- dbSendQuery(conn, "SELECT ITEM_UT as item_ut, AUTHOR_KEYWORD as author_keyword FROM paper_author_keyword")
paperKeyword <- dbFetch(res,n = -1)
dbClearResult(res)
# close db connection
dbDisconnect(conn)

# clean useless object
addPersistentObjects("paperMeta")
addPersistentObjects("paperCitationYear")
addPersistentObjects("paperYearCitedMatrix")
addPersistentObjects("paperAuthor")
addPersistentObjects("paperKeyword")
rmTempObject()

# save .RData
save(file = "paperdb.RData",list = memoryWhiteList)