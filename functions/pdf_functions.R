#==== extract_title ===========================================
extract_title <- function(i_pdf) {
  
  title <- ""
  
  toc <- pdftools::pdf_toc(i_pdf)
  toc <- paste(unlist(toc))
  
  if(length(toc) != 0) {
    
    while (toc[1] == "") {
      toc <- toc[c(-1)]
    }
    
    title <- toc[1]
  }
  
  return(title)
}

#==== search_kw_in_text ===========================================
search_kw_in_text <- function(i_pdf, i_kw) {
  freq <- rep(0, length(i_kw))
  
  txt  <- pdftools::pdf_text(i_pdf)
  
  for (kw in i_kw) {
    for (page in 1:length(txt)) {
      if (kw %in% c("BECS", "BECCS", "AR", "DAC", "EW")) {
        res <- gregexpr(kw, txt[page])[[1]]
      } else {
        res <- gregexpr(kw, tolower(txt[page]))[[1]]
      }
      
      if (res[1] != -1) freq[which(i_kw == kw)] <- freq[which(i_kw == kw)] + length(res)
    }
  }
  
  freq <- data.frame(t(freq))
  names(freq) <- i_kw
  
  freq <- cbind(data.frame(fullpath=i_pdf, stringsAsFactors = FALSE), freq)
  
  return(freq)
  
}

#==== search_doi_in_text ===========================================
search_doi_in_text <- function(i_pdf) {
  
  doi = ""
  
  print(paste(i_pdf))
  
  txt  <- pdftools::pdf_text(paste(i_pdf))
  nb_pages <- pdf_info(i_pdf)$pages
  
  i_page <- 1
  flag_doi_found <- FALSE
  
  while(!flag_doi_found & i_page <= min(nb_pages, 5)) {
    
    txt_doi <- strsplit(txt,"\r\n")[[i_page]][grep("doi", strsplit(tolower(txt),"\r\n")[[1]])]
    
    if (length(txt_doi) != 0) {
      # pos_doi   <- gregexpr("doi", tolower(txt_doi))[[1]][1]
      # pos_space <- gregexpr("\\s", txt_doi)[[1]]
      # 
      # id_space <- which(abs(pos_space - pos_doi) == min (abs(pos_space - pos_doi)))
      # 
      # if (pos_space[id_space] < pos_doi) id_space <- id_space + 1
      # 
      # if (length(test) != 0) {
      #   doi = substr(txt_doi, pos_doi, pos_space[id_space])
      # }
      if (length(txt_doi) > 1) txt_doi <- txt_doi[length(txt_doi)]
      doi = txt_doi
      
      flag_doi_found <- TRUE
      
    } 
    
    i_page <- i_page + 1
  }
  
  if (doi == "") cat(paste0("warning: doi could not be found in document: ", basename(i_pdf),"\n"))
  
  return(doi)
  
}

#==== get_prism_doi ===========================================
get_prism_doi <- function(i_pdfMetadata) {
  
  doi = ""
  
  start = regexpr("<prism:doi>", i_pdfMetadata)
  end   = regexpr("</prism:doi>", i_pdfMetadata)
  
  if (!length(start)==0 && !length(end) == 0) {
    doi = substr(i_pdfMetadata,start+11,end-1)
  }
  
  return(doi)
  
}
