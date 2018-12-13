library(dplyr)
library(tidyr)
library(googlesheets)

googlesheets::gs_auth()
ss <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1sCPMV45U2q5E9eYm7-vJfp9TNKrGIhl34dhEEEMzkok/edit#gid=1473283493")
#gsdf <- googlesheets::gs_read(ss, "Only references")
gsdf_detail <- googlesheets::gs_read(ss, "Only references detailed")


tmp <- df_wholetxt_woref %>% 
  left_join(
    gsdf_detail %>% 
      mutate(rowid = row_number()) %>% 
      left_join(
        mapping,
        by=c("rowid"="gspos")
      ) %>% 
      filter(`PDF?`=="Yes") %>% 
      rename(AB=Abstract) %>% 
      select(AU,PY,TI,AB,DT,SO,DOI,Reference,Link,doc),
    by=c("doc")
  ) %>% 
  rename(FULLTEXT=text) %>% 
  select(AU,PY,TI,AB,FULLTEXT,DT,SO,DOI,Reference,Link,doc)

row.names(tmp) <- NULL  

#write.csv2(tmp, file = "SDGtm_wholedocs_2018-01-30.csv", row.names = FALSE)

# Check data in jango DB
scoping_docs <- read.csv("data/scoping_docs_2018-02-12.csv", stringsAsFactor=FALSE)

original_docs <- tmp 

original_docs$inDB          <- FALSE
original_docs$matchingTitle <- ""
original_docs$sdist         <- ""

for (krow in 1:nrow(original_docs)) {
  
  # Compute string distance
  sdist <- stringdist::stringdist(tolower(original_docs$TI[krow]), tolower(scoping_docs$wosarticle__ti), method = "jaccard", q = 2)
  sdist[which(nchar(scoping_docs$wosarticle__ti) == 0)] <- NA
  
  # Get document title in Django DB
  title <- scoping_docs$wosarticle__ti[which(sdist == min(sdist, na.rm=TRUE))]
  
  # If distance is greater than threshold display message
  if (length(which(sdist == min(sdist, na.rm=TRUE))) == 1) {
    if (sdist[which(sdist == min(sdist, na.rm=TRUE))] > 0.25) {
      cat("Problem?\n")
      cat(paste0("Original: ", original_docs$TI[krow], "\n"))
      cat(paste0("Match   : ", title, "\n"))
      cat("\n")
      original_docs$matchingTitle[krow] <- title
      original_docs$sdist[krow] <- sdist[which(sdist == min(sdist, na.rm=TRUE))]
    } else {
      original_docs$inDB[krow]          <- TRUE
      original_docs$matchingTitle[krow] <- title
      original_docs$sdist[krow] <- sdist[which(sdist == min(sdist, na.rm=TRUE))]
    }
  } else {
    if (length(min(sdist, na.rm=TRUE)) == 0) {
      cat("Could not find matching document in DB\n")
      cat(paste0("Original: ", original_docs$TI[krow], "\n"))
      cat(paste0("Match   : [None]\n"))
      cat("\n")
    } else {
      cat("Severald matching documents in DB\n")
      cat(paste0("Original: ", original_docs$TI[krow], "\n"))
      cat(paste0("Match   : ", title, "\n"))
      cat("\n")
    }
  }
  
}


original_docs$excerpt         <- ""
original_docs$inDB            <- FALSE
original_docs$matchingExcerpt <- ""
original_docs$sdist           <- ""

for (krow in 1:nrow(original_docs)) {
  
  # Compute string distance
  sdist <- stringdist::stringdist(tolower(original_docs$FULLTEXT[krow]), tolower(scoping_docs$fulltext), method = "jaccard", q = 3)
  sdist[which(nchar(scoping_docs$fulltext) == 0)] <- NA
  
  # Get document title in Django DB
  matchingExcerpt <- substr(scoping_docs$fulltext[which(sdist == min(sdist, na.rm=TRUE))], 1, 100)
  
  # If distance is greater than threshold display message
  if (length(which(sdist == min(sdist, na.rm=TRUE))) == 1) {
    if (sdist[which(sdist == min(sdist, na.rm=TRUE))] > 0.25) {
      cat("Problem?\n")
      cat(paste0("Original: ", original_docs$TI[krow], "\n"))
      cat(paste0("Match   : ", title, "\n"))
      cat("\n")
      original_docs$excerpt[krow]         <- substr(original_docs$FULLTEXT[krow], 1, 100)
      original_docs$matchingExcerpt[krow] <- matchingExcerpt
      original_docs$sdist[krow] <- sdist[which(sdist == min(sdist, na.rm=TRUE))]
    } else {
      original_docs$inDB[krow]          <- TRUE
      original_docs$excerpt[krow]         <- substr(original_docs$FULLTEXT[krow], 1, 100)
      original_docs$matchingExcerpt[krow] <- matchingExcerpt
      original_docs$sdist[krow] <- sdist[which(sdist == min(sdist, na.rm=TRUE))]
    }
  } else {
    if (length(min(sdist, na.rm=TRUE)) == 0) {
      cat("Could not find matching document in DB\n")
      cat(paste0("Original: ", substr(original_docs$FULLTEXT[krow], 1, 100), "\n"))
      cat(paste0("Match   : [None]\n"))
      cat("\n")
    } else {
      cat("Severald matching documents in DB\n")
      cat(paste0("Original: ", substr(original_docs$FULLTEXT[krow], 1, 100), "\n"))
      cat(paste0("Match   : ", matchingExcerpt, "\n"))
      cat("\n")
    }
  }
  
  
  
}

