library(xml2)
library(dplyr)

xmlfiles <- list.files("data/acrobat_xml_all/", full.names = TRUE)

get_doc_struct <- function(bml, lvl=NA) { #, secid=NA
  
  spaces <- ifelse(is.na(lvl), "", paste0(rep(" ", lvl*2), collapse=""))
  #print(paste0(spaces, "Level: ", lvl))
  
  ids <- which(names(bml) == "bookmark")
  #print(paste0(spaces, "names: ", paste(names(bml), collapse=", ")))
  
  if (length(ids) == 0) {
    #print(paste0(spaces, "  > No ids found."))
    return(data.frame())
  } else {
    #print(paste0(spaces, "  > ids: ", paste(ids, collapse=",")))
    if (is.na(lvl) && length(ids) == 1) lvl <- 0
    if (is.na(lvl) && length(ids) > 1)  lvl <- 1
    
    
    df_list <- list()
    #print(paste0(spaces, "  > looping over ids..."))
    pos <- 1
    for (kid in ids) {
      # Get title
      title <- bml[[kid]] %>% attr("title")
      
      # # Generate section id
      # if(lvl == 0) {
      #   secid <- "0"
      # } else {
      #   secid <- ifelse(is.na(secid), lvl, paste0(secid, ".", pos))
      # }
      
      # Create data.frame
      df_list[[title]] <- data.frame(
        level = lvl,
        #secid = secid,
        title = title,
        stringsAsFactors = FALSE
      ) %>% rbind(
        get_doc_struct(bml[[kid]], lvl=lvl+1) #, secid=secid)
      )
      
      # Update position
      pos <- pos +1
      
    }
    df <- do.call("rbind", df_list)
    rownames(df) <- NULL
    
  }
  
  
  
  return(df)
}

doc_struct_list <- list()
df_bookmark     <- data.frame()
# Loop over files
for (k in xmlfiles) {
  hasBMT <- FALSE
  
  id <- which(xmlfiles == k)
  
  print(paste0("Processing (", id, "/", length(xmlfiles), "): ", k, "..."))
  
  txt <- readLines(k)
  
  # Detect presence and position of bookmark-tree
  id_start <- grep("<bookmark-tree>",  txt, fixed = TRUE)
  id_end   <- grep("</bookmark-tree>", txt, fixed = TRUE)
  
  if(length(id_start) != 0 && length(id_end) != 0) {
    print(paste0("  > bookmark tree found!"))
    
    # Remove unecessary character that crashes script
    txt <- gsub("&#6;", "", txt)
    
    id_rm <- grep("<destination>", txt, fixed=TRUE)
    ids <- id_start:id_end
    ids <- ids[which(!ids %in% id_rm)]
    
    xml <- read_xml(paste(txt[c(1, ids)], collapse="\n"))
      
    l <- xml %>% xml2::as_list()
    
    doc_struct_list[[k]] <- get_doc_struct(l)
    
    #print(xml %>% xml2::as_list())
    hasBMT <- TRUE
  }
  
  # # Detect presence of H tags
  # if (!hasBMT) {
  #   id_htag <- grep("<H[1-9]>",  txt)
  #   
  #   if (length(id_htag) != 0) {
  #     print(paste0("  > h-tag(s) found!"))
  #     
  #     xml <- read_xml(paste(txt[c(1, ids)], collapse="\n"))
  #     
  #     l <- xml %>% xml2::as_list()
  #     
  #   }
  # 
  # }
  
  
  
  df_bookmark <- df_bookmark %>% 
    rbind(data.frame(
      file = k,
      bookmarktree = hasBMT
    ))
  
}


