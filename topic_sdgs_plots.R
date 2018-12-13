rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)
library(circlize)

library(RPostgreSQL)

source("/home/galm/pg_keys.R")
#source("functions.R")


drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database # note that "con" will be used later in each connection to the database 
con <- dbConnect(drv, dbname = "tmv_app",
                 host = "localhost", port = 5432,
                 user = pg_user, password = pg_pw)

##############################################################
## Get all the papers from the data base
# 658
q <- paste0('SELECT "scoping_doc"."title", "scoping_doc"."PY", "tmv_app_doctopic"."score", "tmv_app_doctopic"."scaled_score", "tmv_app_topic"."title", "tmv_app_topic"."top_words", "tmv_app_topic"."score" 
  FROM "tmv_app_doctopic" 
  INNER JOIN "tmv_app_topic" ON ("tmv_app_doctopic"."topic_id" = "tmv_app_topic"."id") 
  LEFT OUTER JOIN "scoping_doc" ON ("tmv_app_doctopic"."doc_id" = "scoping_doc"."id") 
  WHERE ("tmv_app_doctopic"."run_id" = 658 AND "tmv_app_topic"."run_id_id" = 658)')

alldocs <- data.frame(dbGetQuery(con, q))

# Get unique topics
topics <- unique(alldocs$title.1)

# Generate data.frame mapping topics to SDGs
topicssdgs <- data.frame(
  topic_id   = paste(c(1,2,
                       #2,
                       3,4,5,
                       6,
                       #6,
                       7,8,9,10,11,12,13,14)),
  topic_name = c("Mitigation scenarios",
                 "Carbon pricing & mitigation costs",
                 #"Carbon pricing & mitigation costs",
                 "Sustainable transitions & governance",
                 "Food security",
                 "Land-based mitigation",
                 "Low carbon electricity",
                 "CCS, bioenergy & negative emissions",
                 #"Land-based mitigation",
                 "Air pollution & health",
                 "Low carbon electricity II",
                 "Water availability and consumption",
                 "Energy security",
                 "SSP scenario framework",
                 "CBA of climate policies",
                 "Species abundance & biodiversity"),
  topic_shortname = c(
                 "Mitig. scen.",
                 "C. pric. & mitig. costs",
                 "Sustain. trans. & gov.",
                 "Food security",
                 "Land-based mitig.",
                 "Low carbon elec.",
                 "CCS, bioen. & neg. em.",
                 #"Land-based mitigation",
                 "Air pollution & health",
                 "Low carbon elec. II",
                 "Water avail. and cons.",
                 "Energy security",
                 "SSP scen.",
                 "CBA of climate pol.",
                 "Spec. abund. & biodiv."),
  stemmed_keywords = c(
    "{emiss, scenario, reduct}", 
    "{price, carbon, scenario}",
    #"{price, carbon, scenario}", 
    "{transit, govern, sustain}", 
    "{food, crop, scenario}", 
    "{land, bioenergi, crop}",
    #"{land, bioenergi, crop}",
    "{plant, power, brazilian}",
    "{fulltech, ccs, scenario}", 
    "{pollut, air, emiss}", 
    "{nuclear, technolog, electr}",
    "{water, irrig, withdraw}",
    "{secur, oil, scenario}", 
    "{ssp, ssps, scenario}",
    "{damag, cost, adapt}",
    "{speci, dispers, biodivers}"),
  sdg = c(
    "13", "8", 
    #"8", 
    NA, "2", "13", 
    #"15", 
    "7", 
    "13", "3", "7", "6", NA, NA, "13", "15"))

# Generate list mapping topics to SDGs
mapping_topics_sdgs <-
  list(
    "T1"  = c(13),       # SDG13: Climate action
    "T2"  = c(8),        # SDG8: Decent work and economic growth (& SDG13: Climate action?)
    "T3"  = NA,          
    "T4"  = c(2),        # SDG2: Zero hunger 
    "T5"  = c(13),       # SDG13: Climate action  (& SDG15: Life on land?)
    "T6"  = c(7),        # SDG7: Affordable and clean energy
    "T7"  = c(13),       # SDG13: Climate action
    "T8"  = c(3),        # SDG3: Good health and well being 
    "T9"  = c(7),        # SDG7: Affordable and clean energy
    "T10" = c(6),        # SDG6: Clean water and sanitation
    "T11" = NA,
    "T12" = NA,
    "T13" = c(13),       # SDG13: Climate action
    "T14" = c(15)        # SDG15: Life on land
  )



document_topiclevel_threshold <- 0.1# as.numeric(quantile(alldocs$score, 0.85))

alldocs_sdg <- alldocs %>% 
  left_join(topicssdgs, by=c("title.1"="stemmed_keywords")) %>% 
  filter(score >= document_topiclevel_threshold)

alldocs %>% 
  left_join(topicssdgs, by=c("title.1"="stemmed_keywords")) %>% 
  group_by(topic_name) %>% 
  arrange(desc(score)) %>% 
  mutate(rank=row_number()) %>% 
  ungroup() %>% 
  select(rank,title,PY,score,title.1,topic_name,sdg) %>% 
  View()

topic_table <-lapply(unique(alldocs_sdg$title),
  function(x) {
    
    tmp <- alldocs_sdg %>% 
      filter(title == x)
    
    topic_ids <- as.numeric(unlist(strsplit(paste(tmp$topic_id)[which(!is.na(tmp$topic_id))], ",", fixed=TRUE)))
    
    #ids<-unique(unlist(mapping_topics_sdgs[y]))
    m <- matrix(0,14,14)
    m[topic_ids, topic_ids] <- 1
    
    return(m)
  }) %>% Reduce("+", .)
row.names(topic_table) <- paste0("T",seq(1:14))
colnames(topic_table)  <- paste0("T",seq(1:14))


topic_table_ut <- topic_table
topic_table_ut[which(lower.tri(topic_table_ut, diag=TRUE))] <- 0

# Remove cross-topic links inside SDGs
crosstopic_links <- topicssdgs %>% 
  select(sdg, topic_id) %>% 
  filter(!is.na(sdg)) %>% 
  group_by(sdg) %>% 
  summarise(topics=paste(topic_id, collapse=",")) %>% 
  ungroup() %>% 
  filter(grepl(",", topics, fixed=TRUE))
for (krow in 1:nrow(crosstopic_links)) {
  topic_table_ut[as.numeric(strsplit(crosstopic_links$topics[krow],",",fixed=TRUE)[[1]]), as.numeric(strsplit(crosstopic_links$topics[krow],",",fixed=TRUE)[[1]])] <- 0.0
}

# Generate SDG table
sdg_table = matrix(0,17,17)
for (krow in 1:14) {
  for (kcol in 1:14) {
    ksdg_row <- as.numeric(paste(topicssdgs$sdg[which(as.numeric(paste(topicssdgs$topic_id)) == krow)]))
    ksdg_col <- as.numeric(paste(topicssdgs$sdg[which(as.numeric(paste(topicssdgs$topic_id)) == kcol)]))
    
    if (!is.na(ksdg_row) && !is.na(ksdg_col)) {
      sdg_table[ksdg_row, ksdg_col] <- sdg_table[ksdg_row, ksdg_col] + topic_table_ut[krow,kcol]
    }
  }  
}

# # Make it lower triangle
# for (kcol in 1:17) {
#   for (krow in kcol:17) {
#     sdg_table[krow, kcol] <- sdg_table[krow, kcol] + sdg_table[kcol, krow]
#     sdg_table[kcol, krow] <- 0
#   }
# }
# Make it upper triangle
for (krow in 1:17) {
  for (kcol in krow:17) {
    sdg_table[krow, kcol] <- sdg_table[krow, kcol] + sdg_table[kcol, krow]
    sdg_table[kcol, krow] <- 0
  }
}

rownames(sdg_table) <- paste0("SDG", 1:17)
colnames(sdg_table) <- paste0("SDG", 1:17)

write.csv2(sdg_table, file = "TM_SDGIAM_table.csv")

topic_df <- topic_table_ut %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "from") %>% 
  tidyr::gather(to, value, -from)




topicsdg_df <- lapply(
  1:length(mapping_topics_sdgs),
  function(x) {
    topic_id <- names(mapping_topics_sdgs)[x]
    sdg_ids  <- mapping_topics_sdgs[[x]]
    
    tmp <- data.frame()
    
    if(!is.na(sdg_ids[1])) {
      tmp <- data.frame(
        topic_id = rep(topic_id, length(sdg_ids)),
        sdg_id   = paste0("SDG", paste(sapply(paste(sdg_ids), function(x) ifelse(nchar(x) == 2, x, paste0("0",x))))),
        stringsAsFactors = FALSE
      )
    }
    
    return(tmp)
    
  }) %>% do.call("rbind", .)


topic_df2 <- topic_df %>% 
  filter(!from %in% names(mapping_topics_sdgs)[which(is.na(mapping_topics_sdgs))]) %>% 
  filter(!to %in% names(mapping_topics_sdgs)[which(is.na(mapping_topics_sdgs))]) %>% 
  left_join(topicsdg_df, by=c("from"="topic_id")) %>% 
  rename(sdgid_from=sdg_id) %>% 
  left_join(topicsdg_df, by=c("to"="topic_id")) %>% 
  rename(sdgid_to=sdg_id) %>% 
  mutate(type = "real") %>% 
  mutate(from = paste(from)) %>% 
  mutate(to = paste(to)) %>% 
  filter(value != 0)
  

sum_topic_df2 <-
  topic_df2 %>% 
  group_by(sdgid_from) %>% 
  summarize(value=sum(value)) %>% 
  ungroup()

sum_topic_df2_from <-
  topic_df2 %>% 
  group_by(sdgid_from) %>% 
  summarize(value=sum(value)) %>% 
  ungroup()

sum_topic_df2_to <-
  topic_df2 %>% 
  group_by(sdgid_to) %>% 
  summarize(value=sum(value)) %>% 
  ungroup()
sum_topic_df2_combinedfromto <- sum_topic_df2_from %>% 
  rename(val_from=value) %>% 
  left_join(sum_topic_df2_to %>%
              rename(val_to=value) , 
            by=c("sdgid_from"="sdgid_to")) %>% 
  mutate(value=val_from+val_to)

default_topic_df <- 
  data.frame(
    sdg_id = c(paste0("SDG0", 1:9), paste0("SDG", 10:17)))


topic_minval <- 5 #min(sum_topic_df2_combinedfromto$value) #max(sum_sdg_df2$value)

topic_df3 <- default_topic_df %>% 
  left_join(
    sum_topic_df2,
    by=c("sdg_id"="sdgid_from")
  ) %>% 
  mutate(value=ifelse(is.na(value),topic_minval,value)) %>%  #max(sum_sdg_df2$value)
  rename(total=value) %>% 
  full_join(
    topic_df2,
    by=c("sdg_id"="sdgid_from")) %>% 
  mutate(from = ifelse(is.na(from), "T0", from)) %>% 
  mutate(to = ifelse(is.na(to), "T0", to)) %>% 
  mutate(type = ifelse(is.na(type), "fake", type)) %>% 
  mutate(value = ifelse(type == "fake", total, value)) %>% 
  mutate(sdgid_to = ifelse(is.na(sdgid_to), sdg_id, sdgid_to)) %>% 
  rename(sdgid_from=sdg_id) %>% 
  filter(!(type == "fake" & sdgid_from %in% c("SDG02","SDG03","SDG06","SDG07","SDG08","SDG13","SDG15")))


df <- topic_df3 %>% 
  select(from, to, sdgid_from, sdgid_to, value)
  

sdg_colors <- c(
  "SDG01"  = "#E5243B",
  "SDG02"  = "#DDA63A",
  "SDG03"  = "#4C9F38",
  "SDG04"  = "#C5192D",
  "SDG05"  = "#FF3A21",
  "SDG06"  = "#26BDE2",
  "SDG07"  = "#F9B00B",
  "SDG08"  = "#A21942",
  "SDG09"  = "#FD6925",
  "SDG10"  = "#DD1367",
  "SDG11"  = "#FD9D24",
  "SDG12"  = "#BF8B2E",
  "SDG13"  = "#3F7E44",
  "SDG14"  = "#0A97D9",
  "SDG15"  = "#56C02B",
  "SDG16"  = "#00689D",
  "SDG17"  = "#19486A"
)

all_topics = sort(unique(c(df[[1]], df[[2]])))
color_topics = structure(RColorBrewer::brewer.pal(length(all_topics), "Set3"), names = all_topics)
color_topics[2:length(color_topics)] <- paste0(color_topics[2:length(color_topics)], "ff")
color_topics[1] <- "#00000000" #aste0(color_topics[1], "00")

color_topics <- c(
  "T0"  = "#00000000",  # None
  "T1"  = "#4d4d4dff",  # Mitigation scenario (SDG13)
  "T2"  = "#A21942ff",  # Carbon pricing and mitigation costs (SDG8)
  "T4"  = "#DDA63Aff",  # Food security (SDG2)
  "T5"  = "#808080ff",  # Land-based mitigation (SDG13)
  "T6"  = "#f9b00bff",  # Low carbon electricity (SDG7)
  "T7"  = "#b3b3b3ff",  # CCS, bioenenergy & negative emissions (SDG13)
  "T8"  = "#4C9F38ff",  # Air pollution & health (SDG3)
  "T9"  = "#ffed6fff",  # Low carbon electricity II (SDG7)
  "T10" = "#26BDE2ff",  # Water availability and consumption (SDG6)
  "T13" = "#e6e6e6ff",  # CBA of climate policies (SDG13)
  "T14" = "#56C02Bff"   # Spec. abund. & biodiv. (SDG15)
)

df2 = data.frame(from  = paste(df[[3]], df[[1]], sep = "|"),
                 to  = paste(df[[4]], df[[2]], sep = "|"),
                 value = df[[5]], stringsAsFactors = FALSE)

# Regorganise chords (Priority to climate action SDG13)
for (krow in 1:nrow(df2)) {
  if (strsplit(df2$to[krow], "|", fixed=TRUE)[[1]][1] == "SDG13") {
    tmp <- df2$to[krow]
    df2$to[krow] <- df2$from[krow]
    df2$from[krow] <- tmp
  }
}

# Add labels
df2$labels <- sapply(strsplit(df2$from, "|", fixed=TRUE), function(x) {
  out <- ""
  if (x[2] != "T0") {
    out <- paste(topicssdgs$topic_shortname)[which(topicssdgs$topic_id == substr(x[2],2,2))]
  }
  return(out)
}) %>% unlist()

df_names <- data.frame(
  sdg_topic = sort(unique(c(unique(df2$from), unique(df2$to)))),
  shortname = sapply(strsplit(sort(unique(c(unique(df2$from), unique(df2$to)))), "|", fixed=TRUE), function(x) {
    out <- ""
    if (x[2] != "T0") {
      out <- paste(topicssdgs$topic_shortname)[which(topicssdgs$topic_id == substr(x[2],2,nchar(x[2])))]
    }
    return(out)
  }) %>% unlist(),
  stringsAsFactors = FALSE
)
df_names$value <- 10 # probably useless
combined = unique(data.frame(topics = c(df[[1]], df[[2]]), 
                             sdgs = c(df[[3]], df[[4]]), stringsAsFactors = FALSE))
combined = combined[order(combined$sdgs, combined$topics), ]
order = paste(combined$sdgs, combined$topics, sep = "|")

grid.col = structure(color_topics[combined$topics], names = order)

gap = rep(1, length(order))
gap[which(!duplicated(combined$sdgs, fromLast = TRUE))] = 5

svglite::svglite(file = paste0("topic_sdgs_chord_diagram_thoreshold", paste(document_topiclevel_threshold),"_2018-02-13.svg"))

circos.par(gap.degree = gap, start.degree = 180, clock.wise = TRUE)
chordDiagram(df2, order = order, annotationTrack = c("grid", "axis"),  #, "axis"
             grid.col = grid.col, directional = FALSE,
             preAllocateTracks = list(
               track.height = 0.04,
               track.margin = c(0.15, 0)
             )
)

# Add topic names
circos.track(factors = df_names$sdg_topic,
             track.index = 2,
             bg.border = 0,
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter,
                           CELL_META$ycenter, 
                           df_names$shortname[which(df_names$sdg_topic == CELL_META$sector.index)])
             })

# SDG ring (set colours)
for(ksdg in names(sdg_colors)) {
  l = combined$sdgs == ksdg
  sn = paste(combined$sdgs[l], combined$topics[l], sep = "|")
  highlight.sector(sn, track.index = 1, col = sdg_colors[ksdg], 
                   text = ksdg, niceFacing = TRUE)
}
circos.clear()

# SDG ring (set labels)
labels <- lapply(
  topicssdgs$topic_name[as.numeric(unlist(sapply(names(color_topics)[2:length(names(color_topics))], function(y) which(paste0("T",paste(topicssdgs$topic_id)) == y)[1])))],
  function(x) {
    paste0(x, "\n", topicssdgs$stemmed_keywords[which(topicssdgs$topic_name == x)[1]])
  }
)

legend("bottomleft", pch = 15, col = color_topics[2:length(names(color_topics))], 
       legend = 
         unlist(labels), cex = 0.6)
# legend("bottomright", pch = 15, col = sdg_colors, 
#        legend = names(sdg_colors), cex = 0.6)

dev.off()

pdf(file = paste0("topic_sdgs_chord_diagram_thoreshold", paste(document_topiclevel_threshold),".pdf"), width=10, height=5)

circos.par(gap.degree = gap, start.degree = 90, clock.wise = TRUE)
chordDiagram(df2, order = order, annotationTrack = c("grid", "axis"),  #, "axis"
             grid.col = grid.col, directional = FALSE,
             preAllocateTracks = list(
               track.height = 0.04,
               track.margin = c(0.15, 0)
             )
)

for(ksdg in names(sdg_colors)) {
  l = combined$sdgs == ksdg
  sn = paste(combined$sdgs[l], combined$topics[l], sep = "|")
  highlight.sector(sn, track.index = 1, col = sdg_colors[ksdg], 
                   text = ksdg, niceFacing = TRUE)
}
circos.clear()

labels <- lapply(
  topicssdgs$topic_name[as.numeric(unlist(sapply(names(color_topics)[2:length(names(color_topics))], function(y) which(paste0("T",paste(topicssdgs$topic_id)) == y)[1])))],
  function(x) {
    paste0(x, "\n", topicssdgs$stemmed_keywords[which(topicssdgs$topic_name == x)[1]])
  }
)

legend("bottomleft", pch = 15, col = color_topics[2:length(names(color_topics))], 
       legend = 
         unlist(labels), cex = 0.6)
# legend("bottomright", pch = 15, col = sdg_colors, 
#        legend = names(sdg_colors), cex = 0.6)

dev.off()

#document_topiclevel_threshold <- 0.05

tmp <- alldocs %>% 
  left_join(topicssdgs, by=c("title.1"="stemmed_keywords")) %>% 
  filter(score >= document_topiclevel_threshold) %>% 
  filter(topic_name %in% paste(topicssdgs$topic_name[as.numeric(unlist(sapply(names(color_topics)[2:length(names(color_topics))], function(y) which(paste0("T",paste(topicssdgs$topic_id)) == y)[1])))])) %>% 
  group_by(PY, topic_name, score) %>% 
  summarise(value=sum(score)) %>% 
  ungroup()

fill_cols <- color_topics[2:length(names(color_topics))]
names(fill_cols) <- paste(topicssdgs$topic_name[as.numeric(unlist(sapply(names(color_topics)[2:length(names(color_topics))], function(y) which(paste0("T",paste(topicssdgs$topic_id)) == y)[1])))])

p <- ggplot(data=tmp) +
  geom_bar(aes(x=PY, y=value, fill=topic_name), stat="identity", position="stack") +
  theme_bw() +
  xlab("") + ylab("Topic score") +
  scale_fill_manual(
    values = fill_cols,
    name = "Topics"
  )
print(p)

ggsave(p, filename = paste("topicscore_threshold", paste(document_topiclevel_threshold),".pdf"), width=10, height=5)
