#==== USER SECTION =================================================================
u_path_pdf <- "data/pdfs"
u_path_xml <- "data/xml"


#==== INITIALISE ===================================================================
# load libraries
library(devtools)
library(bibliometrix)
library(SnowballC)
library(servr)
library(httpuv)
library(ggplot2)
library(tm)
#library(wordcloud)
library(dplyr)
library(tidyr)
library(stringr)
library(topicmodels)
#library(igraph)
library(jsonlite)
library(pdftools)
library(wordcloud)
#library(text2vec)
#library(NMF)
library(LDAvis)
library(xml2)

library(scimetrix)

# load functions
source("functions/pdf_functions.R")

# get list of pdfs
v_path_pdfs <- list.files(u_path_pdf, full.names = TRUE)
v_path_xmls <- list.files(u_path_xml, full.names = TRUE)

#==== PROCESS DATA =================================================================
#---- Get pdf content and structure ------------------------------------------------
#-- TODO: Join two lapply methods
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

v_pdf_years <- lapply(v_path_pdfs,
                      function(x) {
                        
                        print(basename(x))

                        # Initialise fields
                        py  <- NA
                        au  <- ""
                        type <- "article"
                        comment <- ""
                                           
                        # Find publication year     
                        tmp <- regexpr(pattern = "([1-2][0,7,8,9][0-9][0-9])", text = basename(x))
                        if (as.numeric(tmp) != -1) {
                          py <- substr(basename(x), as.numeric(tmp), as.numeric(tmp)+3)
                          au <- substr(basename(x), 1, as.numeric(tmp)-2)
                        }
                        
                        # Find publication type
                        tmp <- grep("manual|description|documentation", basename(x), value=T, ignore.case = TRUE)
                        if (length(tmp) != 0) {
                          type <- "model documentation"
                        }
                        if (basename(x) == "Meinshausen et al. 2011 Emulating coupled atmosphere-ocean and carbon cycle models with a simpler model, MAGICC6 – Part 1 Model description and calibration.pdf") type <- "article"
                        
                        # Manual adjustments
                        if (basename(x) == "Berger et al-IJGEI-7.pdf")                               au <- "Berger et al"
                        if (basename(x) == "Berger et al-IJGEI400103.pdf")                           au <- "Berger et al"
                        if (basename(x) == "IFs Agricultural Model Documentation V25.03.pdf")        au <- "Rothman et al."
                        if (basename(x) == "IFs Economic model Documentation v43 clean.pdf")         au <- "Hughes"
                        if (basename(x) == "IFs Energy model Documentation_v9.pdf")                  au <- "Hughes et al."
                        if (basename(x) == "IFs Health model Documentation v49.pdf")                 au <- "Hughes et al."
                        if (basename(x) == "IFs Infrastructure model Documentation v12 - clean.pdf") au <- "Rothman and Irfan"
                        if (basename(x) == "IFs Population_model Documentation_v10.pdf")             au <- "Hughes"
                        
                        if (basename(x) == "Berger et al-IJGEI-7.pdf")                               py <- 2017
                        if (basename(x) == "Berger et al-IJGEI400103.pdf")                           py <- 2017
                        if (basename(x) == "IFs Agricultural Model Documentation V25.03.pdf")        py <- 2017
                        if (basename(x) == "IFs Economic model Documentation v43 clean.pdf")         py <- 2015
                        if (basename(x) == "IFs Energy model Documentation_v9.pdf")                  py <- 2014
                        if (basename(x) == "IFs Health model Documentation v49.pdf")                 py <- 2014
                        if (basename(x) == "IFs Infrastructure model Documentation v12 - clean.pdf") py <- 2013
                        if (basename(x) == "IFs Population_model Documentation_v10.pdf")             py <- 2014
                        
                        if (basename(x) == "Alkemade et al. 2011 Applying GLOBIO at different geographical levels.pdf") type    <- "book chapter"
                        if (basename(x) == "Biemans 2012 Water constraints on future food production.pdf")              type    <- "thesis"
                        if (basename(x) == "Bohl et al. 2016 Understanding and Forecasting Demographic Risk and Benefits.pdf") type  <- "report"
                        if (basename(x) == "Bohl et al. 2017 Development Trends Report for South Africa.pdf")           type <- "report"
                        if (basename(x) == "Bohl et al. 2017 Development Trends Report for Southern Africa.pdf") type <- "report"
                        if (basename(x) == "Bosello et al. 2014 (documentation damage function WITCH model).pdf") type <- "working paper"
                        if (basename(x) == "Bosetti et al. 2009 The 2008 WITCH Model New Model Features and Baseline.pdf") type <- "working paper"
                        if (basename(x) == "Clarke et al 2007.pdf") type <- "report"
                        if (basename(x) == "Cilliers 2013 Assessing Long-Term State Fragility in Africa Prospects for 26 ‘More Fragile’ Countries.pdf") type <- "report"
                        if (basename(x) == "Cilliers 2011 African Futures 2050 The Next 40 Years.pdf") type <- "report"
                        if (basename(x) == "Chateau 2011 Employment Impacts of Climate Change Mitigation Policies in OECD.pdf") type <- "working paper"
                        if (basename(x) == "Cave et al. 2009 Trends in Connectivity Technologies and their Socioeconomic Impacts.pdf") type <- "report"
                        if (basename(x) == "Cantore 2011 Future Paths of Poverty A Scenario Analysis with Integrated Assessment Models.pdf") type <- "working paper"
                        if (basename(x) == "Burt et al. 2014 Eradicating poverty in fragile states prospects of reaching the high-hanging fruit by 2030.pdf") type <- "working paper"
                        if (basename(x) == "Burrows 2016 Reducing the Risks from Rapid Demographic Change.pdf") type <- "report"
                        if (basename(x) == "Goedkoop et al.  2014 ReCiPe 2008 (version 1.08)—report 1 characterisation.pdf") type <- "report"
                        if (basename(x) == "Esbaugh et al. 2011 Taps and Toilets How Greater Access Can Radically Improve Africa’s Future.pdf") type <- "working paper"
                        if (basename(x) == "Durand-Lasserve et al. 2015 Modelling of distributional impacts of energy subsidy reforms.pdf") type <- "working paper"
                        if (basename(x) == "Dellink 2017 International trade consequences of climate change.pdf") type <- "working paper"
                        if (basename(x) == "Dellink 2014 Consequences of Climate Change Damages for Economic Growth.pdf") type <- "working paper"
                        if (basename(x) == "Dellink 2010 Costs, Revenues, and Effectiveness of the Copenhagen Accord Emission Pledges for 2020.pdf") type <- "working paper"
                        if (basename(x) == "Hughes 2007 Forecasting Global Economic Growth with Endogenous Multifactor Productivity The International Futures (IFs) Approach.pdf") type <- "working paper"
                        if (basename(x) == "Hughes 2006 UNEP GEO4 Driver Scenarios Using IFS with Pardee.pdf") type <- "working paper"
                        if (basename(x) == "Hughes 2006 Assessing the Credibility of Forcasts using International Futures (IFs) Verification and Validation.pdf") type <- "working paper"
                        if (basename(x) == "Hughes 2005 Scenario Analysis with International Futures") type <- "working paper"
                        if (basename(x) == "Hughes 2004 The Base Case of International Futures (IFs) Comparison with Other Forecasts.pdf") type <- "working paper"
                        if (basename(x) == "Hughes 2004 Forecasting the Human Development Index.pdf") type <- "working paper"
                        if (basename(x) == "Hilderink et al. 2008 Towards a Global Integrated Sustainability Model GISMO1.0 status report.pdf") type <- "report, model documentation"
                        if (basename(x) == "Hedden et al. 2016 Ending hunger in Africa The elimination of hunger and food insecurity on the African by 2025 Conditions for success.pdf") type <- "report"
                        if (basename(x) == "Hedden et al. 2013 Fracking for Shale Gas in South Africa Blessing or Curse.pdf") type <- "working paper"
                        if (basename(x) == "Havlík et al. 2015 Global climate change, food supply and livestock production systems A bioeconomic analysis.pdf") type <- "book chapter"
                        if (basename(x) == "Havlik et al. 2015 Climate change impacts and mitigation in the developing world an integrated assessment of the agriculture and forestry sectors.pdf") type <- "working paper"
                        if (basename(x) == "Hughes 2008 Assessing Strategies for Reducing Global Poverty.pdf") type <- "working paper"
                        if (basename(x) == "Lanzi et al. 2013 Addressing Competitiveness and Carbon Leakage Impacts Arising from Multiple Carbon Markets.pdf") type <- "working paper"
                        if (basename(x) == "Labat et al. 2015 GECO2015 Global Energy and Climate Outlook Road to Paris Assessment of Low Emission Levels under World Action Integrating National Contributions.pdf") type <- "report"
                        if (basename(x) == "Kok et al. (2014) How sectors can contribute to sustainable use and conservation of biodiversity.pdf") type <- "report"
                        if (basename(x) == "Kiesewetter et al. 2016 A Scalable Approach to Modelling Health Impacts of Air Pollution Based on Globally Available Data.pdf") type <- "abstract"
                        if (basename(x) == "Irfan 2012 SADC Higher Education Futures 2050.pdf") type <- "report"
                        if (basename(x) == "International Council for Science (2017).pdf") type <- "report"
                        if (basename(x) == "International Commission on Financing Global Education Opportunity 2016 The Learning Generation Investing in Education for a Changing World.pdf") type <- "report"
                        if (basename(x) == "IDDRI (2014) DDPP_report.pdf") type <- "report"
                        if (basename(x) == "Hughes et al. 2011 Vulnerability to Interstate Conflict Evaluating Quantitative Measures.pdf") type <- "report"
                        if (basename(x) == "Hughes et al. 2011 Projections of Global Health Outcomes from 2005 to 2060 Using the International Futures Integrated Forecasting Tool.pdf") type <- "working paper"
                        if (basename(x) == "Hughes et al. 2011 Forecasting the Impacts of Environmental Constraints on Human Development.pdf") type <- "report"
                        if (basename(x) == "Hughes 2013 Development-Oriented Policies and Alternative Human Development Paths Aggressive but Reasonable Interventions.pdf") type <- "report"
                        if (basename(x) == "Hughes 2009 Economic Futures and Their Implications for Global Development The Unfolding of the Great Recession.pdf") type <- "working paper"
                        if (basename(x) == "OECD 2012 OECD Environmental Outlook to 2050 consequences of inaction.pdf") type <- "report"
                        if (basename(x) == "OECD 2016 Policy-Highlights-Economic-consequences-of-outdoor-air-pollution-web.pdf") type <- "report"
                        if (basename(x) == "OECD 2015 The Economic Consequences of Climate Change.pdf") type <- "report"
                        if (basename(x) == "OECD 2009 The Economics of Climate Change Mitigation Policies and Options for Global Action Beyond 2012_ES.pdf") type <- "report (executive summary only)"
                        if (basename(x) == "OECD 2008 OECD Environmental Outlook to 2030.pdf") type <- "report"
                        if (basename(x) == "Nelson et al. 2010 Food security, farming, and climate change to 2050, Scenarios, results, policy options.pdf") type <- "report"
                        if (basename(x) == "Narayan 2016 Envisioning a Healthy Future Africa's Shifting Burden of Disease") type <- "working paper"
                        if (basename(x) == "Moyer et al. 2015 Advancing development in Uganda evaluating policy choices for 2016-21 and selected impacts to 2040.pdf") type <- "working paper"
                        if (basename(x) == "Moyer 2012 Cultivating the Future Exploring the Potential and Impact of a Green Revolution in Africa.pdf") type <- "working paper"
                        if (basename(x) == "Messner 1995 User's guide for MESSAGE III.pdf") type <- "model documentation"
                        if (basename(x) == "Marczak 2016 Latin America and the Caribbean 2030 Future Scenarios.pdf") type <- "report"
                        if (basename(x) == "Shepherd et al. 2014 The Chronic Poverty Report 2014-2015 The Road to Zero Extreme Poverty.pdf") type <- "report"
                        if (basename(x) == "Shepherd et al. 2013 The Geography of Poverty, Disasters and Climate Extremes in 2030.pdf") type <- "report"
                        if (basename(x) == "Scott et al. 2017 Modeling Artificial Intelligence and Exploring its Impact.pdf") type <- "working paper"
                        if (basename(x) == "Riahi et al. 2012 GEA-Summary-web.pdf") type <- "report (executive summary only)"
                        if (basename(x) == "Riahi et al. 2012 GEA_Chapter17_pathways_lowres.pdf") type <- "book chapter"
                        if (basename(x) == "PSI 2012 (MergeDescription).pdf") type <- "model documentation"
                        if (basename(x) == "PSI 2014 (MergeCalibration).pdf") type <- "model documentation"
                        if (basename(x) == "pbl-2012-roads-from-rio-pathways-to-achieve-global-sustainability-goals-by-2050.pdf") type <- "report"
                        if (basename(x) == "PBL 2011 The protein puzzle the consumption and production of meat, dairy and fish in the European Union.pdf") type <- "report"
                        if (basename(x) == "PBL 2010 Rethinking Global Biodiversity Strategies Exploring structural changes in production and consumption to reduce biodiversity loss.pdf") type <- "report"
                        if (basename(x) == "PBL 2009 Beyond 2015 Long-term development and the Millennium Development Goals.pdf") type <- "report"
                        if (basename(x) == "PBL (2017) People_and_the_Earth_WEB.pdf") type <- "report"
                        if (basename(x) == "WMO_UNEP(2011).pdf") type <- "report"
                        if (basename(x) == "Van Vuuren 2007 Energy systems and climate policy - Long-term scenarios for an uncertain future.pdf") type <- "report"

                        # Comments
                        if (basename(x) == "Alkemade et al. 2011 Applying GLOBIO at different geographical levels.pdf") comment <- "uncomplete"
                        if (basename(x) == "Hayashi et al. 2014 Evaluation of Global Energy Crop Production Potential up to 2100 under Socioeconomic Development and Climate Change Scenarios.pdf") comment <- "japanese text"
                        if (basename(x) == "Kiesewetter et al. 2016 A Scalable Approach to Modelling Health Impacts of Air Pollution Based on Globally Available Data.pdf") comment <- "uncomplete, abstract only"
                        if (basename(x) == "Hughes et al. 2014 Opportunities and Challenges of a World with Negligible Senescence.pdf") comment <- "duplicated"
                        if (basename(x) == "Hughes et al. 2011 Projections of Global Health Outcomes from 2005 to 2060 Using the International Futures Integrated Forecasting Tool.pdf") comment <- "japanese, french, arabic, russian text"
                        if (basename(x) == "OECD 2009 The Economics of Climate Change Mitigation Policies and Options for Global Action Beyond 2012_ES.pdf") comment <- "uncomplete"
                        if (basename(x) == "Riahi et al. 2012 GEA-Summary-web.pdf") comment <- "uncomplete"
                        if (basename(x) == "Riahi et al. 2012 GEA_Chapter17_pathways_lowres.pdf") comment <- "uncomplete"
                        if (basename(x) == "Zhou et al. 2017 An Analysis on Hypothetical Shocks Representing Cooling Water Shortage Using a Computable General Equilibrium Model.pdf") comment <- "research gate paper"
                        if (basename(x) == "Yillia P (2015)") comment <- "german text"
                        
                        data.frame(
                          author   = au,
                          year     = py,
                          type     = type,
                          comment  = comment,
                          filename = basename(x),
                          path     = x
                        )
                      }) %>% 
  do.call("rbind", .)

v_pdf_summary <- inner_join(
  v_pdf_years,
  v_pdf_summary %>% 
    select(title,doi,pages,path),
  by=c("path")
) %>% 
  select(author, year, type, title, doi, pages, comment, filename, path) %>% 
  mutate(year = as.numeric(paste(year)))

v_pdf_summary <- v_pdf_summary %>% 
  mutate(hasXML = ifelse(filename %in% substr(unique(basename(v_data_xml_df$doc)), 1, nchar(unique(basename(v_data_xml_df$doc)))-4), TRUE, FALSE))

set.seed(123456)
v_literature_sampleJan <- rbind(
  v_pdf_summary %>% filter(hasXML == TRUE, type == "article") %>% sample_n(4),
  v_pdf_summary %>% filter(hasXML == TRUE, type == "working paper") %>% sample_n(3),
  v_pdf_summary %>% filter(hasXML == TRUE, type == "report") %>% sample_n(3))
write.csv2(v_literature_sampleJan, file = "SDGTM_literature_sample_forJan.csv")
v_data_xmlforJan1 <- v_data_xml_df %>% 
  mutate(pdf_doc = substr(basename(doc), 1, nchar(basename(doc))-4)) %>% 
  filter(pdf_doc %in% v_literature_sampleJan$filename)
write.csv2(v_data_xmlforJan1, file = "SDGTM_xml_structure_forJan.csv")
v_data_xmlforJan2 <- v_data_xml_df_1doc_per_doc %>% 
  mutate(pdf_doc = substr(basename(doc), 1, nchar(basename(doc))-4)) %>% 
  filter(pdf_doc %in% v_literature_sampleJan$filename)
write.csv(v_data_xmlforJan2, file = "SDGTM_xml_collapsed_forJan.csv", quote = TRUE)
set.seed(123)
v_literature_sampleJerome <- rbind(
  v_pdf_summary %>% filter(hasXML == TRUE, type == "article") %>% sample_n(4),
  v_pdf_summary %>% filter(hasXML == TRUE, type == "working paper") %>% sample_n(3),
  v_pdf_summary %>% filter(hasXML == TRUE, type == "report") %>% sample_n(3))
write.csv2(v_literature_sampleJerome, file = "SDGTM_literature_sample_forJerome.csv")
v_data_xmlforJerome1 <- v_data_xml_df %>% 
  mutate(pdf_doc = substr(basename(doc), 1, nchar(basename(doc))-4)) %>% 
  filter(pdf_doc %in% v_literature_sampleJerome$filename)
write.csv2(v_data_xmlforJerome1, file = "SDGTM_xml_structure_forJerome.csv")
v_data_xmlforJerome2 <- v_data_xml_df_1doc_per_doc %>% 
  mutate(pdf_doc = substr(basename(doc), 1, nchar(basename(doc))-4)) %>% 
  filter(pdf_doc %in% v_literature_sampleJerome$filename)
write.csv2(v_data_xmlforJerome2, file = "SDGTM_xml_collapsed_forJerome.csv")

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

save(v_pdf_summary, v_data_xml, v_data_xml_df, file = "data/processedData.RData")

v_data_xml_df_1doc_per_doc <- v_data_xml_df %>%
  select(doc, role, text) %>%
  filter(role == "body-text") %>% 
  group_by(doc) %>%
  summarise(text = paste(text, collapse="")) %>%
  ungroup()

v_data_xml_df_1doc_per_doc <- v_data_xml_df_1doc_per_doc %>%
  mutate(text=tolower(text)) %>% 
  mutate(text=gsub("", ""))
  #mutate(text=iconv(text, to="utf8"))

#---- Topic modelling --------------------------------------------------------------
ignorewords = c("the", "with")

# make a corpus
#corpus <- corporate(v_data_xml_df_1doc_per_doc, col="text")
ignoreWords <- c("the", "however", "this", "and")
corpus <- tm::Corpus(tm::VectorSource(v_data_xml_df_1doc_per_doc$text)) %>% 
  #tm::tm_map(tm::PlainTextDocument) %>% 
  tm::tm_map(tm::removePunctuation) %>% 
  tm::tm_map(tm::removeNumbers) %>% 
  tm::tm_map(tm::removeWords, tm::stopwords()) %>% 
  tm::tm_map(tm::removeWords, ignoreWords) %>% 
  tm::tm_map(tm::stemDocument)
#t <- iconv(t,to="utf-8-mac")

# make a doc-term matrix and refresh the corpus to reflect any docs removed in the process
dtm <- makeDTM(corpus,0.95,v_data_xml_df_1doc_per_doc$doc,0.01,0)

rem         <- filter(v_data_xml_df_1doc_per_doc, doc %in% dtm$removed)
docs_used    <- subset(v_data_xml_df_1doc_per_doc, !(doc %in% dtm$removed))
corpus_used <- refresh_corp(dtm$dtm)

# save data
docs   <- docs_used
corpus <- corpus_used
save(docs,corpus,dtm,file="data/docs.RData")

rm(docs_used,corpus_used,rem)

SEED <- 2016

system.time({
  LDA_model = LDA(dtm$dtm,k=60, method="VEM",
                   control=list(seed=SEED))
})

# saves the results into the working dir
save(LDA_model, file="data/lda_model_60topics.RData")
terms(LDA_model, 3)
terms(LDA_model, 10) %>% as.data.frame() %>% write.csv2(file = "output/LDAmodel_60topics_crosssection.csv")

visualise(LDA_model, corpus, dtm$dtm, dir="output/LDA_model")

k_tw = 1
VERBOSE=TRUE

# window topic modeling
wtm <- list()
wtm[[k_tw]] <- list()

# Select documents
wtm[[k_tw]]$data <- v_data_xml_df_1doc_per_doc #%>% filter(PY %in% as.numeric(tw[[k_tw]]))

cat(paste0("Number of documents in current time-window: ", nrow(wtm[[k_tw]]$data), "\n"))

# Generator iterator
wtm[[k_tw]]$it <- get_tokensIterator(wtm[[k_tw]]$data, ignorewords, verbose=VERBOSE)

# Create vocabulary
wtm[[k_tw]]$vocab <- create_vocab(wtm[[k_tw]]$it, term_count_min = 5L, verbose=VERBOSE)

# Create Document-Term Matrix
wtm[[k_tw]]$dtm <- generate_dtm(wtm[[k_tw]]$vocab, wtm[[k_tw]]$it, wtm[[k_tw]]$data, verbose=VERBOSE)

# Perform NMF to get topics
tm_nmf <- nmf(as.matrix(wtm[[k_tw]]$dtm), rank = 50, seed="nndsvd")

tm_lda <- LDA$new(n_topics = 60)
doc_topic_distr <- tm_lda$fit_transform(wtm[[k_tw]]$dtm, n_iter = 20)
tm_lda$plot()

#==== PLOT DATA ====================================================================
