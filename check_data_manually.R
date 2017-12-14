#==== For Jan ========================================
set.seed(123456)
v_literature_sampleJan <- rbind(
  v_pdf_summary %>% filter(hasXML == TRUE, type == "article") %>% sample_n(4),
  v_pdf_summary %>% filter(hasXML == TRUE, type == "working paper") %>% sample_n(3),
  v_pdf_summary %>% filter(hasXML == TRUE, type == "report") %>% sample_n(3))
write.csv2(v_literature_sampleJan, file = file.path("manual check", "check_forJan", "SDGTM_literature_sample_forJan.csv"))

v_data_xmlforJan1 <- v_data_xml_df %>% 
  mutate(pdf_doc = substr(basename(doc), 1, nchar(basename(doc))-4)) %>% 
  filter(pdf_doc %in% v_literature_sampleJan$filename)

fnames <- substr(basename(unique(v_data_xmlforJan1$doc)), 1, nchar(basename(unique(v_data_xmlforJan1$doc)))-4)
for (kpdf in fnames) {
  cur_rmd <- file.path("manual check", "check_forJan", paste0(substr(kpdf,1,nchar(kpdf)-4), ".Rmd"))
  file.copy("generate_list_docs.Rmd", cur_rmd)
  cur_dt <- v_literature_sampleJan$type[which(v_literature_sampleJan$filename == kpdf)]
  rmarkdown::render(cur_rmd, params = list(
    doctype = cur_dt,
    data = v_data_xmlforJan1 %>% filter(pdf_doc == kpdf)
  ))
}

# write.csv2(v_data_xmlforJan1, file = "SDGTM_xml_structure_forJan.csv")
# v_data_xmlforJan2 <- v_data_xml_df_1doc_per_doc %>% 
#   mutate(pdf_doc = substr(basename(doc), 1, nchar(basename(doc))-4)) %>% 
#   filter(pdf_doc %in% v_literature_sampleJan$filename)
# write.csv(v_data_xmlforJan2, file = "SDGTM_xml_collapsed_forJan.csv", quote = TRUE)


#==== For Jerome ========================================
set.seed(123)
v_literature_sampleJerome <- rbind(
  v_pdf_summary %>% 
    mutate(pdf_doc = basename(paste(filename))) %>% 
    filter(!pdf_doc %in% paste(v_literature_sampleJan$filename)) %>% 
    filter(hasXML == TRUE, type == "article") %>% 
    sample_n(4),
  v_pdf_summary %>% 
    mutate(pdf_doc = basename(paste(filename))) %>% 
    filter(!pdf_doc %in% paste(v_literature_sampleJan$filename)) %>% 
    filter(hasXML == TRUE, type == "working paper") %>% 
    sample_n(3),
  v_pdf_summary %>% 
    mutate(pdf_doc = basename(paste(filename))) %>% 
    filter(!pdf_doc %in% paste(v_literature_sampleJan$filename)) %>% 
    filter(hasXML == TRUE, type == "report") %>% 
    sample_n(3))
write.csv2(v_literature_sampleJerome, file = file.path("manual check", "check_forJerome", "SDGTM_literature_sample_forJerome.csv"))

v_data_xml_df <- v_data_xml %>% 
  do.call("rbind", .)
v_data_xmlforJerome1 <- v_data_xml_df %>% 
  mutate(pdf_doc = substr(basename(doc), 1, nchar(basename(doc))-4)) %>% 
  filter(pdf_doc %in% v_literature_sampleJerome$filename)

fnames <- substr(basename(unique(v_data_xmlforJerome1$doc)), 1, nchar(basename(unique(v_data_xmlforJerome1$doc)))-4)
for (kpdf in fnames) {
  cur_rmd <- file.path("manual check", "check_forJerome", paste0(substr(kpdf,1,nchar(kpdf)-4), ".Rmd"))
  file.copy("generate_list_docs.Rmd", cur_rmd)
  cur_dt <- v_literature_sampleJerome$type[which(v_literature_sampleJerome$filename == kpdf)]
  rmarkdown::render(cur_rmd, params = list(
    doctype = cur_dt,
    data = v_data_xmlforJerome1 %>% filter(pdf_doc == kpdf)
  ))
}

# write.csv2(v_data_xmlforJerome1, file = "SDGTM_xml_structure_forJerome.csv")
# v_data_xmlforJerome2 <- v_data_xml_df_1doc_per_doc %>% 
#   mutate(pdf_doc = substr(basename(doc), 1, nchar(basename(doc))-4)) %>% 
#   filter(pdf_doc %in% v_literature_sampleJerome$filename)
# write.csv2(v_data_xmlforJerome2, file = "SDGTM_xml_collapsed_forJerome.csv")