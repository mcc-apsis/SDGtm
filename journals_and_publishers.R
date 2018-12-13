library(googlesheets)
library(ggplot2)

#---- Get doc info from google sheet -----------------------------------------------
gs <- gs_url("https://docs.google.com/spreadsheets/d/1sCPMV45U2q5E9eYm7-vJfp9TNKrGIhl34dhEEEMzkok")
gs_docsDetailed <- googlesheets::gs_read(gs, "Only references detailed")
save(gs_docsDetailed, file="data/gs_docsDetailed.RData")

publishers <- read.csv2("publishers.csv", stringsAsFactors = FALSE)

p <- ggplot() +
  geom_bar(aes(x=reorder(DT, freq), y=freq, fill=`XML?`), 
           stat = "identity", position = "stack", 
           data = gs_docsDetailed %>% 
             filter(`PDF?` == "Yes") %>% 
             group_by(DT, `XML?`) %>% 
             summarize(freq=n()) %>% 
             ungroup() %>% 
             arrange(desc(freq))) +
  theme_bw() +
  xlab("") + ylab("") +
  coord_flip()

plot(p)

p <- ggplot() +
  geom_bar(aes(x=reorder(SO, freq), y=freq, fill=`XML?`), 
           stat = "identity", position = "stack", 
           data = gs_docsDetailed %>% 
             filter(`PDF?` == "Yes", DT == "article") %>% 
             group_by(SO, `XML?`) %>% 
             summarize(freq=n()) %>% 
             ungroup() %>% 
             arrange(desc(freq)) %>% 
             left_join(publishers, by=c("SO"="journal"))) +
  theme_bw() +
  xlab("") + ylab("") +
  coord_flip()

plot(p)


p <- ggplot() +
  geom_bar(aes(x=reorder(publisher, freq), y=freq, fill=`XML?`), 
           stat = "identity", position = "stack", 
           data = gs_docsDetailed %>% 
             filter(`PDF?` == "Yes", DT == "article") %>% 
             group_by(SO, `XML?`) %>% 
             summarize(freq=n()) %>% 
             ungroup() %>% 
             arrange(desc(freq)) %>% 
             left_join(publishers, by=c("SO"="journal")) %>% 
             group_by(publisher, `XML?`) %>% 
             summarize(freq=sum(freq)) %>% 
             ungroup() %>% 
             arrange(desc(freq))) +
  theme_bw() +
  xlab("") + ylab("") +
  coord_flip()

plot(p)