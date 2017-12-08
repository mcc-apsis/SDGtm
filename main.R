#==== USER SECTION =================================================================
u_path_pdf <- "data/pdfs"
u_path_xml <- "data/xml"


#==== INITIALISE ===================================================================
# load libraries
library(tidyverse)
library(pdftools)
library(xml2)
library(scientometrix)

# load functions
source("functions/pdf_functions.R")

# get list of pdfs
v_path_pdfs <- list.files(u_path_pdf, full.names = TRUE)
v_path_xmls <- list.files(u_path_xml, full.names = TRUE)

#==== PROCESS DATA =================================================================
#---- Get pdf content and structure ------------------------------------------------
v_pdf_summary <- lapply(v_path_pdfs,
       function(x) {
         
         print(basename(x))
         
         # get pdf infos
         info <- pdf_info(x)
         
         # get publishing year
         py <- ifelse(is.null(info$created), "", format(info$created, "%Y"))
         
         # get number of pages
         pages <- ifelse(is.null(info$pages), "", info$pages)
         
         # Additional info
         # get author info
         au    <- ifelse(is.null(info$keys$Author), "", info$keys$Author)
         
         # get title
         title <- ifelse(is.null(info$keys$Title), "", info$keys$Title)
         
         if (title == "") {
           title <- extract_title(x)
         }
         
         # get doi
         doi   <- ifelse(is.null(info$keys$doi), "", info$keys$doi)
         
         if (doi == "" & pages <= 40) {
           doi <- search_doi_in_text(x)
         }
         if (doi == "" & pages <= 40) {
           doi <- get_prism_doi(x)
         }
         
         if (grepl("doi:", tolower(doi))) {
           doi <- trimws(strsplit(trimws(substr(doi, as.numeric(regexpr("doi:", tolower(doi)))+4, nchar(doi))), " ", fixed=T)[[1]][1])
         }

         if (grepl("doi.org", tolower(doi))) {
           doi <- trimws(strsplit(trimws(substr(doi, as.numeric(regexpr("doi.org", tolower(doi)))+8, nchar(doi))), " ", fixed=T)[[1]][1])
         }
                  
         if (grepl("doi", tolower(doi))) {
           doi <- trimws(strsplit(trimws(substr(doi, as.numeric(regexpr("doi", tolower(doi)))+3, nchar(doi))), " ", fixed=T)[[1]][1])
         }
         
         data.frame(
           author   = au,
           year     = py,
           title    = title,
           doi      = doi,
           pages    = pages,
           filename = basename(x),
           path     = x
         )
       }) %>% 
  do.call("rbind", .)

v_data_xml <- read_xml(v_path_xmls[1])

v_data_xml <- lapply(v_path_xmls,
       function(x) {
         
         print(x)
         
         #---- local function --------
         get_mcatt_sectionheader <- function(i_df, i_attr, i_levels) {
           tmp <- i_df %>% 
             filter(grepl("^[1-9]{1}(\\.{1}[1-9]{1}){,2}\\s{1}(\\w){1,}", text) & (maxy-miny) <= 20) %>% 
             filter(row_number() <= 5) %>% 
             select_(i_attr) %>% 
             group_by_(i_attr) %>% 
             summarise(count=n()) %>% 
             ungroup() %>% 
             arrange(desc(count)) %>% 
             filter(row_number() <= i_levels)
           
           return(tmp[[i_attr]])
         }
         get_mcatt_bodytext <- function(i_df, i_attr) {
           tmp <- i_df %>% 
             arrange(desc(ncar)) %>% 
             filter(row_number() <= 5) %>% 
             select_(i_attr) %>% 
             group_by_(i_attr) %>% 
             summarise(count=n()) %>% 
             ungroup() %>% 
             arrange(desc(count)) %>% 
             filter(row_number() == 1)
           
           return(tmp[[i_attr]])
         }
         
         tmp <- xml_find_all(read_xml(x), "//page/paragraph[@role='section-heading' or 
                                                  @role='unknown'         or 
                                                  @role='body-text'       or 
                                                  @role='figure-caption'  or 
                                                  @role='figure'          or 
                                                  @role='table-caption'   or 
                                                  @role='table-caption']") 
         
         if (length(tmp) != 0) {
           print("  > Processing file...")
           page <- xml_attr(tmp, "page")
           minx <- xml_attr(tmp, "minX")
           miny <- xml_attr(tmp, "minY")
           maxx <- xml_attr(tmp, "maxX")
           maxy <- xml_attr(tmp, "maxY")
           mcfo <- xml_attr(tmp, "mostCommonFont")
           mcfs <- xml_attr(tmp, "mostCommonFontsize")
           sfon <- xml_attr(tmp, "startFont")
           sfos <- xml_attr(tmp, "startFontsize")
           efon <- xml_attr(tmp, "endFont")
           efos <- xml_attr(tmp, "endFontsize")
           mcco <- xml_attr(tmp, "mostCommonColor")
           scol <- xml_attr(tmp, "startColor")
           ecol <- xml_attr(tmp, "endColor")
           role <- xml_attr(tmp, "role")
           text <- xml_text(tmp)
           
           df <- data.frame(
             id   = 1:length(text),
             doc  = x,
             page = as.numeric(page),
             minx = as.numeric(minx),
             miny = as.numeric(miny),
             maxx = as.numeric(maxx),
             maxy = as.numeric(maxy),
             mcfo = mcfo,
             mcfs = as.numeric(mcfs),
             sfon = sfon,
             sfos = as.numeric(sfos),
             efon = efon,
             efos = as.numeric(efos),
             mcco = mcco,
             scol = scol,
             ecol = ecol,
             role = role,
             text = text,
             ncar = nchar(text),
             stringsAsFactors = FALSE
           )
           
           # Infer most common font, font-size and colour used for section headers
           print("  > Infer most common font for section headers")
           section_header_mcfont     <- get_mcatt_sectionheader(df, "mcfo", 2)
           section_header_mcfontsize <- as.numeric(get_mcatt_sectionheader(df, "mcfs", 2))
           section_header_mccolour   <- get_mcatt_sectionheader(df, "mcco", 1)
           
           # Infer most common font, font-size and colour used for body text
           print("  > Infer most common font for body text")
           body_text_mcfont     <- get_mcatt_bodytext(df, "mcfo")
           body_text_mcfontsize <- as.numeric(get_mcatt_bodytext(df, "mcfs"))
           body_text_mccolour   <- get_mcatt_bodytext(df, "mcco")
           
           # Infer pdf content and structure (section headers and body text) by filtering out rows that do not match those common properties
           print("  > Infer pdf structure and content")
           df <- rbind(
             df %>% 
               filter(grepl("^[1-9]{1}(\\.{1}[1-9]{1}){,2}\\s{1}(\\w){1,}", text) & (maxy-miny) <= 20) %>% 
               filter(mcfo %in% section_header_mcfont, mcfs %in% section_header_mcfontsize, mcco %in% section_header_mccolour) %>% 
               mutate(role="section-heading"),
             df %>% 
               filter(!grepl("^[1-9]{1}(\\.{1}[1-9]{1}){,2}\\s{1}(\\w){1,}", text) & (maxy-miny) > 20) %>% 
               filter(mcfo == body_text_mcfont, mcfs == body_text_mcfontsize, mcco == body_text_mccolour) %>% 
               mutate(role = "body-text")) %>% 
             arrange(id) %>% 
             mutate(role=ifelse(grepl("^abstract.*", tolower(text)), "abstract", role))
           
           # Allocate sections to rows #1
           print("  > Allocate sections #1")
           df <- df %>% 
             mutate(section = ifelse(role == "abstract", "abstract", "")) %>% 
             mutate(section = ifelse(role == "section-heading", text, ""))
           
           # Allocate sections to rows #2
           print("  > Allocate sections #2")
           df$section[which(df$role == "body-text")] <- sapply((df %>% 
                                                                  select(id, role) %>% 
                                                                  filter(role == "body-text"))$id, 
                                                               function(y) {
                                                                 (df %>% 
                                                                    select(id, role, text) %>% 
                                                                    filter(role == "section-heading")
                                                                  )$text[max(which((y - (df %>% 
                                                                                           select(id, role) %>% 
                                                                                           filter(role == "section-heading"))$id) > 0))]})
             
           
           
           
         } else {
           print("===========> Skipping <==================================================================")
           df <- data.frame(
             id   = 1,
             doc  = x,
             page = NA,
             minx = NA,
             miny = NA,
             maxx = NA,
             maxy = NA,
             mcfo = "",
             mcfs = NA,
             sfon = "",
             sfos = NA,
             efon = "",
             efos = NA,
             mcco = "",
             scol = "",
             ecol = "",
             role = "",
             text = "",
             ncar = 0,
             section = "",
             stringsAsFactors = FALSE
           )
         }

         
         return(df)
         
       })

v_data_xml_df <- v_data_xml %>% 
  do.call("rbind", .)

#---- Topic modelling --------------------------------------------------------------


#==== PLOT DATA ====================================================================