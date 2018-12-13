library(dplyr)
library(tidyr)
library(googlesheets)

googlesheets::gs_auth()
ss <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1sCPMV45U2q5E9eYm7-vJfp9TNKrGIhl34dhEEEMzkok/edit#gid=1473283493")
gsdf <- googlesheets::gs_read(ss, "Only references")
gsdf_detail <- googlesheets::gs_read(ss, "Only references detailed")

#---- Topic modelling --------------------------------------------------------------
ignorewords = c("the", "with")

# make a corpus
#corpus <- corporate(v_data_xml_df_1doc_per_doc, col="text")
ignoreWords <- c("the", "however", "this", "and")
corpus <- tm::Corpus(tm::VectorSource(gsdf$Abstract)) %>% 
  #tm::tm_map(tm::PlainTextDocument) %>% 
  tm::tm_map(tm::removePunctuation) %>% 
  tm::tm_map(tm::removeNumbers) %>% 
  tm::tm_map(tm::removeWords, tm::stopwords()) %>% 
  tm::tm_map(tm::removeWords, ignoreWords) %>% 
  tm::tm_map(tm::stemDocument)
#t <- iconv(t,to="utf-8-mac")

# make a doc-term matrix and refresh the corpus to reflect any docs removed in the process
dtm <- scimetrix::makeDTM(corpus,0.95,gsdf$Reference,0.05271,0)

rem         <- filter(gsdf, Reference %in% dtm$removed)
docs_used    <- subset(gsdf, !(Reference %in% dtm$removed))
corpus_used <- scimetrix::refresh_corp(dtm$dtm)

# save data
docs   <- docs_used
corpus <- corpus_used
save(docs,corpus,dtm,file="data/gsAbs_20180105.RData")

rm(docs_used,corpus_used,rem)

SEED <- 2016

tm_sdg_gsAbs_20180121 <- list()

for (knt in c(10,15,20)) {
  print(paste0("Running LDA with ", knt, " topics..."))
  system.time({
    tm_sdg_gsAbs_20180121[[paste(knt)]] = topicmodels::LDA(dtm$dtm,k=knt, method="VEM",
                                              control=list(seed=SEED))
  })
  
}

save(tm_sdg_gsAbs_20180121, file="data/lda_model_gsAbs_topics_10-15_20180121.RData")
file.copy("topic_summary_template.Rmd", "topic_summary_abstracts.Rmd", overwrite = T)

rmarkdown::render("topic_summary_abstracts.Rmd", params = list(
  tms=tm_sdg_gsAbs_20180121
))


for (knt in c(10,15,20)) {
  print(paste0("Exporting Top 10 Terms for LDA model with ", knt, " topics..."))
  LDA_model <- tm_sdg_gsAbs_20180121[[paste(knt)]]
  topicmodels::terms(LDA_model, 10) %>% 
    as.data.frame() %>% 
    write.csv2(file = paste0("output/LDAmodel_gsAbs_", knt, "topics_crosssection.csv"))
}



# saves the results into the working dir
#save(LDA_model, file="data/lda_model_60topics.RData")
#terms(LDA_model, 3)



# mapping <- lapply(
#   docxfiles,
#   function(x) {
#     
#     k <- substr(basename(x), 1, nchar(basename(x))-5)
#     
#     sdist <- stringdist::stringdist(k, gsdf_detail$Reference, method="jaccard", q=2)
#     
#     id <- which(sdist == min(sdist))
#     
#     if(length(id) == 1) {
#       ref <- gsdf_detail$Reference[id]
#     } else {
#       
#       tmp <- paste(gsdf_detail$AU, gsdf_detail$PY)
#       sdist <- stringdist::stringdist(k, tmp, method="jaccard", q=1)
#       
#       id2 <- which(sdist == min(sdist))
#       
#       if(length(id2) == 1) {
#         ref <- gsdf_detail$Reference[id2]
#       } else {
#         cat(paste("Warning Could not find associated file for", k, " - (id=", length(id), ", id2=", length(id2), ")\n"))
#       }
#     }
#     
#     
#     
#     out <- data.frame(
#       docxfilename = x,
#       filename     = k,
#       gsreference  = ref,
#       stringsAsFactors = FALSE
#     )
#     
#     return(out)
#     
#   }) %>% do.call("rbind", .)