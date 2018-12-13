library(dplyr)
library(tidyr)
library(stringr)
library(xml2)

docxfiles <- list.files("data/acrobat_docx_all/", full.names = TRUE)

manual_structure <- read.csv2("bookmartree_detail.csv")

doc_struct_list <- doc_struct_list[-which(names(doc_struct_list) == "data/acrobat_xml_all/Burrows 2016 Reducing the Risks from Rapid Demographic Change.xml")]
doc_struct_list <- doc_struct_list[-which(names(doc_struct_list) == "data/acrobat_xml_all/Cherp, 2012.xml")]
doc_struct_list <- doc_struct_list[-which(names(doc_struct_list) == "data/acrobat_xml_all/de Bruin et al. 2009 AD-DICE an implementation of adaptation in the DICE model.xml")]
doc_struct_list <- doc_struct_list[-which(names(doc_struct_list) == "data/acrobat_xml_all/Hanson et al. 2011 A global ranking of port cities with high exposure to climate extremes.xml")]

find_sections_in_text <- function(i_docxtxt, i_sections, DEBUG=FALSE) {
  # Initialise
  match_txtstruct <- list()
  
  # Loop over sections
  for (ksec in i_sections) {
    if (DEBUG) cat(paste0("Processing section (", which(i_sections == ksec),"/", length(i_sections),"): ", ksec,"\n"))
    # Initialise
    flag_stringdist <- FALSE   # Flag indicating the method used (i.e. grep or stringdist)
    
    # Count number of words in current section title
    nwords <- as.numeric(gregexpr("[[:alpha:]]+", ksec)[[1]]) %>% length()
    if (DEBUG) cat(paste0("N-words: ", nwords,"\n"))
    
    # Try to find best match
    if (nwords == 1) {
      # TODO: may want to switch that to a simple grep
      id <- grep(tolower(paste0("^",ksec)), tolower(i_docxtxt))
      if (length(id) == 0) {
        dist <- sapply(i_docxtxt, function(t) {stringdist::stringdist(ksec,t,method = "jaccard",q=1)})
        id <- as.numeric(which(dist == min(dist))[1])
        flag_stringdist <- TRUE
        if (DEBUG) cat(paste0("Method: string-distance jaccard 1-gram","\n"))
      } else {
        if (DEBUG) cat(paste0("Method: grep","\n"))
      }
    } else {
      dist <- sapply(substr(i_docxtxt, 1, nchar(ksec)), function(t) {stringdist::stringdist(ksec,t,method = "jaccard",q=2)})
      id <- as.numeric(which(dist == min(dist))[1])
      flag_stringdist <- TRUE
      if (DEBUG) cat(paste0("Method: string-distance jaccard 2-gram","\n"))
    }
    
    # Test validity of best match by checking line position
    if (DEBUG) cat(paste0("Current position(s): ", paste(id, collapse=", "),"\n"))
    if (flag_stringdist) { 
      if (which(i_sections == ksec) > 1) {
        prev_pos <- as.numeric(match_txtstruct[[i_sections[which(i_sections == ksec)-1]]]$line)
        if (DEBUG) cat(paste0("Previous position: ", prev_pos, " (", i_sections[which(i_sections == ksec)-1],")\n"))
        #itr <- which(dist == min(dist))
        if(id < prev_pos) print(paste0("===========> Warning! wrong sequence of sections <============  ", ksec))
        # while(id < prev_pos) {
        #   itr <- itr+1
        #   id  <- as.numeric(which(dist == min(dist))[itr])
        # }
      }
    } else {
      if (which(i_sections == ksec) > 1) {
        prev_pos <- as.numeric(match_txtstruct[[i_sections[which(i_sections == ksec)-1]]]$line)
        if (DEBUG) cat(paste0("Previous position: ", prev_pos, " (", i_sections[which(i_sections == ksec)-1],")\n"))
        
        if (length(prev_pos) != 0) {
          print(paste0("===========> Warning! could not find previous position <============  ", ksec))
        } else {
          if(id[1] < prev_pos) print(paste0("===========> Warning! wrong sequence of sections <============  ", ksec))
          
          if (length(id) > 1) {
            prev_pos <- as.numeric(match_txtstruct[[i_sections[which(i_sections == ksec)-1]]]$line)
            ids <- id
            itr <- 1
            id <- ids[1]
            
            while(id < prev_pos && itr <= length(ids)) {
              itr <- itr+1
              id  <- ids[itr]
            } 
          }
        }
      }
    }
    
    # Special case for abstracts
    if (tolower(ksec) == "abstract") {
      id <- grep("^abstract", tolower(i_docxtxt))
    }
    
    # Special case for acknowledgments
    if (flag_stringdist) {
      if (tolower(ksec) == "acknowledgments") {
        if (dist[id] >= 0.35) {
          id <- grep("^acknowledgments", tolower(i_docxtxt))
        }
      }
      if (tolower(ksec) == "acknowledgements") {
        if (dist[id] >= 0.35) {
          id <- grep("^acknowledgements", tolower(i_docxtxt))
        }
      }        
    }
    
    # Save information in data frame
    if (DEBUG) cat(paste0("flag_stringdist: ", flag_stringdist,"\n"))
    if (DEBUG) cat(paste0("pattern: ", ksec,"\n"))
    if (DEBUG) cat(paste0("line   : ", id,"\n"))
    if (DEBUG) cat(paste0("text   : ", substr(i_docxtxt[id], 1, nchar(ksec)),"\n"))
    if (DEBUG) cat(paste0("sdist  : ", ifelse(flag_stringdist, dist[id], NA),"\n"))
    match_txtstruct[[ksec]] <- data.frame(
      pattern = ksec,
      line    = id,
      text    = substr(i_docxtxt[id], 1, nchar(ksec)),
      sdist   = ifelse(flag_stringdist, dist[id], NA),
      stringsAsFactors = FALSE
    )
    
    if (DEBUG) cat(paste0("\n\n"))
    
  }  
  
  # Bind data frames together
  match_txtstruct <- do.call("rbind", match_txtstruct)
  rownames(match_txtstruct) <- NULL
  
  return(match_txtstruct)
}

processed_docs <- list()

for (k in docxfiles[177:384]) {
  cat("-----------------------------------------------------\n")
  cat(paste0(k ,"\n"))
  cat("-----------------------------------------------------\n")
  
  flag_case1 <- FALSE
  flag_case2 <- FALSE

  # Case 1: document structure has been extracted from the associated xml file using the bookmark-tree
  if (any(basename(k) %in% paste0(substr(basename(names(doc_struct_list)), 1, nchar(basename(names(doc_struct_list)))-3), "docx"))) {
    cat(paste0("> Case 1\n"))
    id <- which(paste0(substr(basename(names(doc_struct_list)), 1, nchar(basename(names(doc_struct_list)))-3), "docx") == basename(k))
    tmp_struct <- doc_struct_list[[id]] 
    
    if (min(tmp_struct$level) == 0) {
      tmp_struct <- tmp_struct %>% filter(level == 1) 
    } else {
      tmp_struct <- tmp_struct %>% filter(level %in% c(1,2)) 
    }
    
    # Exceptions
    if (basename(k) %in% c("Buhl & Acosta (2016).docx")) tmp_struct <- tmp_struct %>% filter(level == 1) 
    
    tmp_struct <- tmp_struct %>% 
      filter(!title %in% c("Outline placeholder")) %>% 
      filter(!grepl("Tab[0-9]", title))
    
    # Read in text
    docxtxt <- textreadr::read_docx(k) %>% iconv(to='ASCII//TRANSLIT')
    
    # Remove irrelevant text
    docxtxt <- docxtxt[which(!grepl("e-mail", docxtxt))]
    docxtxt <- docxtxt[which(!grepl("correspondance", docxtxt))]
    
    # Remove citations (partial removal for now)
    docxtxt <- gsub("[A-Z]([A-Za-z])+\\s\\([0-9]{4}\\) | 
                     [A-Z]([A-Za-z])+\\sand\\s[A-Z]([A-Za-z])+\\s\\([0-9]{4}\\) | 
                     [A-Z]([A-Za-z])+\\set\\sal\\.\\s\\([0-9]{4}\\) | 
                     [A-Z]{1}([A-Za-z])+\\set\\sal\\.\\s[0-9]{4}    |
                     [A-Z]{1}([a-z])+\\set\\sal\\. | 
                     [A-Z]+\\s[0-9]{4}", "", docxtxt)
    
    # Find sections in text
    match_txtstruct <- find_sections_in_text(docxtxt, tmp_struct$title)
    
    print(match_txtstruct)
    
    structured_doc <- match_txtstruct %>% 
      rename(section = pattern) %>% 
      rename(match   = text) %>% 
      mutate(file=basename(k)) %>% 
      select(file,section,line,match,sdist) %>% 
      mutate(text = "")
    
    for (ksec in 1:(length(structured_doc$section)-1)) {
      if (structured_doc$section[ksec] == "Abstract") {
        id_start <- structured_doc$line[which(structured_doc$section == structured_doc$section[ksec])]
      } else {
        id_start <- structured_doc$line[which(structured_doc$section == structured_doc$section[ksec])] +1
      }
      id_end   <- structured_doc$line[which(structured_doc$section == structured_doc$section[ksec+1])]-1
      
      structured_doc$text[which(structured_doc$section == structured_doc$section[ksec])] <- paste(docxtxt[id_start:id_end], collapse=" ")
      
    }
    
    processed_docs[[k]] <- structured_doc %>% 
      filter(!tolower(section) %in% c("acknowledgments", "acknowledgements", "supplementary data", "references")) %>% 
      filter(!grepl("appendi", tolower(section)))
    
    flag_case1 = TRUE
    
  }
  
  # Case 2: document structure has been manually extracted
  id <- which(basename(substr(manual_structure$file,1 , nchar(paste(manual_structure$file))-4)) == substr(basename(k),1,nchar(basename(k))-5))
  if (flag_case1 == FALSE & length(id) != 0) { # paste(manual_structure$manual[id]) != ""
    print("Case 2")
    
    # Read in text
    docxtxt <- textreadr::read_docx(k) %>% iconv(to='ASCII//TRANSLIT')
    
    # Remove irrelevant text
    docxtxt <- docxtxt[which(!grepl("e-mail", docxtxt))]
    docxtxt <- docxtxt[which(!grepl("correspondance", docxtxt))]
    docxtxt <- docxtxt[which(!grepl("published", docxtxt))]
    
    # Remove citations (partial removal for now)
    docxtxt <- gsub("[A-Z]([A-Za-z])+\\s\\([0-9]{4}\\) | 
                    [A-Z]([A-Za-z])+\\sand\\s[A-Z]([A-Za-z])+\\s\\([0-9]{4}\\) | 
                    [A-Z]([A-Za-z])+\\set\\sal\\.\\s\\([0-9]{4}\\) | 
                    [A-Z]{1}([A-Za-z])+\\set\\sal\\.\\s[0-9]{4}    |
                    [A-Z]{1}([a-z])+\\set\\sal\\. | 
                    [A-Z]+\\s[0-9]{4}", "", docxtxt)
    
    # Find sections in text
    tmp_manualstruct <- strsplit(paste(manual_structure$manual[id]), ";")[[1]] %>% trimws()
    match_txtstruct <- find_sections_in_text(docxtxt, tmp_manualstruct)
    
    print(match_txtstruct)
    
    structured_doc <- match_txtstruct %>% 
      rename(section = pattern) %>% 
      rename(match   = text) %>% 
      mutate(file=basename(k)) %>% 
      select(file,section,line,match,sdist) %>% 
      mutate(text = "")
    
    for (ksec in 1:(length(structured_doc$section)-1)) {
      if (structured_doc$section[ksec] == "Abstract") {
        id_start <- structured_doc$line[which(structured_doc$section == structured_doc$section[ksec])]
      } else {
        id_start <- structured_doc$line[which(structured_doc$section == structured_doc$section[ksec])] +1
      }
      id_end   <- structured_doc$line[which(structured_doc$section == structured_doc$section[ksec+1])]-1
      
      structured_doc$text[which(structured_doc$section == structured_doc$section[ksec])] <- paste(docxtxt[id_start:id_end], collapse=" ")
      
    }
    
    processed_docs[[k]] <- structured_doc %>% 
      filter(!tolower(section) %in% c("acknowledgments", "acknowledgements", "Supplementary data", "references", "bibliography", "orcid ids", "notes")) %>% 
      filter(!grepl("appendix", tolower(section)))
   
    flag_case2 = TRUE
     
  }
  
  # Case 3: 
  if (!flag_case1 & !flag_case2) {
    print("Case 3")
    
    docxtxt <- textreadr::read_docx(k) %>% iconv(to='ASCII//TRANSLIT')
    
    # Remove irrelevant text
    docxtxt <- docxtxt[which(!grepl("e-mail", docxtxt))]
    docxtxt <- docxtxt[which(!grepl("correspondance", docxtxt))]
    
    # Remove citations (partial removal for now)
    docxtxt <- gsub("[A-Z]([A-Za-z])+\\s\\([0-9]{4}\\) | 
                    [A-Z]([A-Za-z])+\\sand\\s[A-Z]([A-Za-z])+\\s\\([0-9]{4}\\) | 
                    [A-Z]([A-Za-z])+\\set\\sal\\.\\s\\([0-9]{4}\\) | 
                    [A-Z]{1}([A-Za-z])+\\set\\sal\\.\\s[0-9]{4}    |
                    [A-Z]{1}([a-z])+\\set\\sal\\. | 
                    [A-Z]+\\s[0-9]{4}", "", docxtxt)
    
    match_txtstruct <- lapply(c("abstract", "introduction", "methods", "results", "discussion", "conclusion", "ackowledgments", "ackowledgements", "references", "bilbiography"), function(x) {
      
      dist <- sapply(docxtxt, function(t) {stringdist::stringdist(tolower(x),tolower(t),method = "jaccard",q=1)})
      
      id <- which(dist == min(dist))[1]
      
      if (tolower(x) == "abstract") {
        id <- grep("^abstract", tolower(docxtxt))
      }
      
      data.frame(
        pattern = x,
        line    = id,
        text    = substr(docxtxt[id], 1, nchar(x)),
        sdist   = dist[id],
        stringsAsFactors = FALSE
      )
    }) %>% do.call("rbind", .)
    rownames(match_txtstruct) <- NULL
    
    match_txtstruct <- match_txtstruct %>% 
      filter(sdist <= 0.35)
    
    
    match_txtstruct <- find_sections_in_text(docxtxt, c("abstract", "introduction", "methods", "results", "discussion", "conclusion", "ackowledgments", "ackowledgements", "references", "bilbiography"), DEBUG=T)
    
    print(match_txtstruct)
    
    
    
    structured_doc <- match_txtstruct %>% 
      rename(section = pattern) %>% 
      rename(match   = text) %>% 
      mutate(file=basename(k)) %>% 
      select(file,section,line,match,sdist) %>% 
      mutate(text = "")
    
    for (ksec in 1:(length(structured_doc$section)-1)) {
      if (tolower(structured_doc$section[ksec]) == "abstract") {
        id_start <- structured_doc$line[which(structured_doc$section == structured_doc$section[ksec])]
      } else {
        id_start <- structured_doc$line[which(structured_doc$section == structured_doc$section[ksec])] +1
      }
      id_end   <- structured_doc$line[which(structured_doc$section == structured_doc$section[ksec+1])]-1
      
      structured_doc$text[which(structured_doc$section == structured_doc$section[ksec])] <- paste(docxtxt[id_start:id_end], collapse=" ")
      
    }
    
    processed_docs[[k]] <- structured_doc %>% 
      filter(!tolower(section) %in% c("acknowledgments", "acknowledgements", "references", "bibliography", "orcid ids", "notes")) %>% 
      filter(!grepl("appendi", tolower(section)))
    
    
  }
  
  # Export to txt file
  nrow <- nrow(processed_docs[[k]])
  if (nrow == 1) {
    cat("======================================================\n", file = paste0("data/processed_docs/", substr(basename(k),1,nchar(basename(k))-4), "txt"), append=FALSE)
    cat(paste0(processed_docs[[k]]$section[1], "\n"), file = paste0("data/processed_docs/", substr(basename(k),1,nchar(basename(k))-4), "txt"), append=TRUE)
    cat("======================================================\n", file = paste0("data/processed_docs/", substr(basename(k),1,nchar(basename(k))-4), "txt"), append=TRUE)
    cat(paste0(processed_docs[[k]]$text[1], "\n\n"), file = paste0("data/processed_docs/", substr(basename(k),1,nchar(basename(k))-4), "txt"), append=TRUE)
  } else {
    for (krow in 2:nrow) {
      cat("======================================================\n", file = paste0("data/processed_docs/", substr(basename(k),1,nchar(basename(k))-4), "txt"), append=TRUE)
      cat(paste0(processed_docs[[k]]$section[krow], "\n"), file = paste0("data/processed_docs/", substr(basename(k),1,nchar(basename(k))-4), "txt"), append=TRUE)
      cat("======================================================\n", file = paste0("data/processed_docs/", substr(basename(k),1,nchar(basename(k))-4), "txt"), append=TRUE)
      cat(paste0(processed_docs[[k]]$text[krow], "\n\n"), file = paste0("data/processed_docs/", substr(basename(k),1,nchar(basename(k))-4), "txt"), append=TRUE)
    }
  }
  
  # # Tidy up
  # file.remove(tmp_zip)
  # unlink(tmp_dir, recursive = TRUE)
  
  cat("\n\n")
}



# # Copy original docx file and transform into zip file
# tmp_copy <- paste0(substr(k, 1, nchar(k)-4), "_copy.docx")
# tmp_zip  <- paste0(substr(k, 1, nchar(k)-4), "_copy.zip")
# 
# file.copy(k, tmp_copy)
# file.rename(tmp_copy, tmp_zip)
# 
# # Unzip file
# tmp_dir <- file.path(dirname(tmp_zip), "tmp_zipcontent")
# dir.create(tmp_dir, showWarnings = FALSE)
# unzip(tmp_zip,exdir=tmp_dir, overwrite = TRUE)


# if (file.exists(file.path(tmp_dir, "word", "header1.xml"))) {
#   h1   <- xml_find_all(read_xml(file.path(tmp_dir, "word", "header1.xml")), "//w:t") %>% xml_contents() %>% paste() %>% paste(collapse="")
#   dist <- sapply(docxtxt, function(t) {stringdist::stringdist(h1,docxtxt,method = "jaccard",q=1)})
#   id   <- which(dist == min(dist))[1]
#   docxtxt <- 
# }
# if (file.exists(file.path(tmp_dir, "word", "header2.xml"))) h2 <- xml_find_all(read_xml(file.path(tmp_dir, "word", "header2.xml")), "//w:t") %>% xml_contents() %>% paste() %>% paste(collapse="")
# if (file.exists(file.path(tmp_dir, "word", "footer1.xml"))) f1 <- xml_find_all(read_xml(file.path(tmp_dir, "word", "footer1.xml")), "//w:t") %>% xml_contents() %>% paste() %>% paste(collapse="")
# if (file.exists(file.path(tmp_dir, "word", "footer2.xml"))) f2 <- xml_find_all(read_xml(file.path(tmp_dir, "word", "footer2.xml")), "//w:t") %>% xml_contents() %>% paste() %>% paste(collapse="")
# 

# match_txtstruct <- lapply(tmp_struct$title, function(x) {
#   
#   dist <- sapply(docxtxt, function(t) {stringdist::stringdist(x,t,method = "jaccard",q=1)})
#   
#   id <- which(dist == min(dist))[1]
#   
#   if (tolower(x) == "abstract") {
#     id <- grep("^abstract", tolower(docxtxt))
#   }
#   
#   out <- data.frame(
#     pattern = x,
#     line    = id,
#     text    = substr(docxtxt[id], 1, nchar(x)),
#     sdist   = dist[id],
#     stringsAsFactors = FALSE
#   )
#   
#   return(out)
# }) %>% do.call("rbind", .)
# rownames(match_txtstruct) <- NULL



# # Open xml file containing document content and styles
# 
# tmp_stl <- file.path(tmp_dir, "word", "styles.xml")
# if (!file.exists(tmp_stl)) stop()
# 
# tmp_xml <- file.path(tmp_dir, "word", "document.xml")
# if (!file.exists(tmp_xml)) stop()
#   
# # xml <- x %>% 
# #   readLines() %>% 
# #   paste(collapse="") %>% 
# #   enc2utf8() %>% 
# #   iconv() %>% 
# #   read_xml()
# 
# # Get styles
# stl <- read_xml(tmp_stl)
# ns_stl <- xml_ns(stl)
# 
# # Get document content
# doc <- read_xml(tmp_xml)
# ns_doc <- xml_ns(doc)
# 
# # w:document >
# # w:body     >
# # w:p        > paragraphs / sections in Word
# # w:r        > runs (i.e. a series of text with the same font, color, etc)
# # w:t        > text 
# 
# styles <- stl %>% xml_find_all("//w:style[@w:type='paragraph' or w:type='character']", ns=ns_stl) %>% xml_attr("w:styleId", ns=ns_stl)
# styles <- stl %>% xml_find_all("//w:style[@w:type='paragraph' or w:type='character']/w:name", ns=ns_stl) %>% xml_attr("w:val", ns=ns_stl)
# 
# # Bookmarks are very helpful to quickly capture the structure of documents
# bookmarks <- xml_find_all(doc, paste0("//w:bookmarkStart"), ns_doc) %>% 
#   xml_attr("w:name", ns_doc)
# bookmarks <- bookmarks[which(!grepl("_bookmark", bookmarks) &
#                              !duplicated(bookmarks))]
# 
# has_abstract <- grep("^abstract$", tolower(bookmarks))
# has_intro    <- grep("^introduction$", tolower(bookmarks))
# has_result   <- grep("result", tolower(bookmarks))
# has_discuss  <- grep("discussion", tolower(bookmarks))
# has_conc     <- grep("conclusion", tolower(bookmarks))
# has_ack      <- grep("^ackowledgments$", tolower(bookmarks))
# has_refs     <- grep("^references$", tolower(bookmarks))
# 
# df_bookmark_style <- lapply(bookmarks, function(x) {
#   
#   res <- xml_find_all(doc, paste0("//w:p[./w:bookmarkStart/@w:name='", x, "']/w:pPr/w:pStyle"), ns_doc) %>% 
#     xml_attr("w:val", ns_doc)
#   
#     df <- data.frame(
#       bookmark = x,
#       style    = ifelse(length(res) != 0, res, ""),
#       stringsAsFactors = FALSE
#     )
#   return(df)
# }) %>% do.call("rbind", .)
# 
# abstract <- xml_find_all(doc, paste0("//w:p[./w:bookmarkStart/@w:name='Abstract']/w:r/w:t"), ns_doc) %>% 
#             xml_contents() %>% 
#             paste(collapse="")
# 
# 
# 
# for (ks in styles) {
#   tmp <- xml_find_all(doc, paste0("//w:p[.//w:pStyle/@w:val=', ks,']"), ns=ns_doc)
#   
#   tmp <- tmp %>% xml_find_all("//w:t")
#   
#   print()
#   
# }
# 
# tmp <- xml_find_all(xml, "//w:p[.//w:pStyle/@w:val='Heading1']", ns=ns_doc) 
# 
# 
# 
