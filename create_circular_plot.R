#devtools::install_git("https://github.com/jokergoo/circlize.git")
library(circlize)

labelled_data <- readxl::read_xlsx("output/LDAmodel_20171214_30topics_crosssection_labelled.xlsx")

map_topics_top3w <- lapply(
  paste0("t", 1:30), 
  function(x) {
    data.frame(
      topic_id   = labelled_data[[x]][which(labelled_data$category == "topic_nb")],
      topic_name = labelled_data[[x]][which(labelled_data$category == "topic_name")],
      top3w      = labelled_data[[x]][which(labelled_data$category %in% c("word1", "word2", "word3"))] %>% paste(collapse=", ") %>% paste0("{", ., "}"),
      stringsAsFactors = TRUE)
    }) %>% 
  do.call("rbind", .)

topcors <- scimetrix::topCors(tm_sdg_20171214[["30"]])
layout <- igraph::layout.fruchterman.reingold(topcors)


model <- tm_sdg_20171214[["30"]]
gamma <- as.data.frame(model@gamma)
topic_names <- paste0("{", terms(model, 10)[1, ], ", ", 
                      terms(model, 10)[2, ], ", ", terms(model, 10)[3, ], 
                      "}")
names(gamma) <- topic_names
cors <- cor(gamma[, ])
cors[lower.tri(cors, diag = TRUE)] <- 0
cors[cors < 0] <- 0
colnames(cors) <- topic_names
topic_cors <- cors
df_topic_cors <- topic_cors %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("topic1") %>% 
  gather(topic2, value, -topic1) %>% 
  filter(value != 0.0)

igraph::V(topcors)$colour <- rainbow(30)
igraph::V(topcors)$topic_name <- paste(map_topics_top3w$topic_name)
igraph::E(topcors)$weight <- igraph::get.edge.attribute(topcors)$weight

igraph::plot.igraph(topcors, layout=layout,
                    vertex.label = igraph::V(topcors)$topic_name,
                    vertex.color = igraph::V(topcors)$colour,
                    edge.width = igraph::E(topcors)$weight
                      )

igraph::write.graph(topcors, file = "topcors.gml")

df_topcors <- igraph::get.edgelist(topcors) %>% 
  as.data.frame(stringsAsFactors=TRUE)
names(df_topcors) <- c("from", "to")
df_topcors <- df_topcors %>% 
  mutate(from = paste(from)) %>% 
  mutate(to   = paste(to)) %>% 
  mutate(value = igraph::get.edge.attribute(topcors)$weight) %>% 
  mutate(from_topicnb = sapply(from, function(x) map_topics_top3w$topic_id[which(map_topics_top3w$top3w == x)])) %>% 
  mutate(to_topicnb = sapply(to, function(x) map_topics_top3w$topic_id[which(map_topics_top3w$top3w == x)])) %>% 
  mutate(from_topicname = sapply(from, function(x) map_topics_top3w$topic_name[which(map_topics_top3w$top3w == x)])) %>% 
  mutate(to_topicname = sapply(to, function(x) map_topics_top3w$topic_name[which(map_topics_top3w$top3w == x)]))

#gap.after = do.call("c", lapply(table(brand), function(i) c(rep(2, i-1), 8)))

circos.par(gap.degree=1, cell.padding = c(0, 0, 0, 0)) #gap.after = gap.after, 

chordDiagram(df_topcors[,c(6,7,3)], 
             directional = 0, 
             #annotationTrack = "grid", 
             preAllocateTracks = list(list(track.height = 0.02)))

circos.track(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.index = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), mean(ylim), sector.index, col = "white", cex = 0.6, facing = "inside", niceFacing = TRUE)
}, bg.border = NA)

circos.clear()

df_topic_cors <- df_topic_cors %>% 
  mutate(from_topicnb = sapply(topic1, function(x) map_topics_top3w$topic_id[which(map_topics_top3w$top3w == x)])) %>% 
  mutate(to_topicnb = sapply(topic2, function(x) map_topics_top3w$topic_id[which(map_topics_top3w$top3w == x)])) %>% 
  mutate(from_topicname = sapply(topic1, function(x) map_topics_top3w$topic_name[which(map_topics_top3w$top3w == x)])) %>% 
  mutate(to_topicname = sapply(topic2, function(x) map_topics_top3w$topic_name[which(map_topics_top3w$top3w == x)]))
circos.clear()

col_fun <- colorRamp2(c(min(df_topic_cors$value), 0.08, max(df_topic_cors$value)), c("#FFEFEF", "#FFcccc", "#FF0000"), transparency = 0.5)

circos.par(gap.degree=1, cell.padding = c(0, 0, 0, 0)) #gap.after = gap.after, 

chordDiagram(df_topic_cors[,c(6,7,3)], 
             grid.col = rep(NA,length(map_topics_top3w$topic_id)),# rainbow(length(map_topics_top3w$topic_id)),
             col = col_fun,
             directional = 0, 
             #annotationTrack = "grid", 
             preAllocateTracks = list(list(track.height = 0.02)))

circos.track(track.index = 2, panel.fun = function(x, y) {
  
  xlim         = get.cell.meta.data("xlim")
  ylim         = get.cell.meta.data("ylim")
  sector.index = get.cell.meta.data("sector.index")
  
  circos.text(mean(xlim), mean(ylim), sector.index, col = "black", cex = 0.6, facing = "inside", niceFacing = TRUE)
}, 
bg.border = NA)

circos.track(track.index = 1, panel.fun = function(x, y) {
  
  xlim         = get.cell.meta.data("xlim")
  ylim         = get.cell.meta.data("ylim")
  sector.index = get.cell.meta.data("sector.index")
  circos.rect(xlim[1], ylim[1], xlim[2], ylim[2]*)
  circos.text(mean(xlim), mean(ylim), sector.index, col = "black", cex = 0.4, facing = "inside", niceFacing = TRUE)
}, 
bg.border = NA)

for(b in unique(brand)) {
  model = names(brand[brand == b])
  highlight.sector(sector.index = model, track.index = 1, col = brand_color[b], 
                   text = b, text.vjust = -1, niceFacing = TRUE)
}

circos.clear()

df = read.table(textConnection("
 brand_from model_from brand_to model_to
      VOLVO        s80      BMW  5series
        BMW    3series      BMW  3series
      VOLVO        s60    VOLVO      s60
      VOLVO        s60    VOLVO      s80
        BMW    3series     AUDI       s4
       AUDI         a4      BMW  3series
       AUDI         a5     AUDI       a5
"), header = TRUE, stringsAsFactors = FALSE)

brand = c(structure(df$brand_from, names=df$model_from), structure(df$brand_to,names= df$model_to))
brand = brand[!duplicated(names(brand))]
brand = brand[order(brand, names(brand))]
brand_color = structure(2:4, names = unique(brand))
model_color = structure(2:8, names = names(brand))

gap.after = do.call("c", lapply(table(brand), function(i) c(rep(2, i-1), 8)))
circos.par(gap.after = gap.after, cell.padding = c(0, 0, 0, 0))

chordDiagram(df[, c(2, 4)], order = names(brand), grid.col = model_color,
             directional = 1, annotationTrack = "grid", preAllocateTracks = list(
               list(track.height = 0.02))
)

circos.track(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.index = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), mean(ylim), sector.index, col = "white", cex = 0.6, facing = "inside", niceFacing = TRUE)
}, bg.border = NA)

for(b in unique(brand)) {
  model = names(brand[brand == b])
  highlight.sector(sector.index = model, track.index = 1, col = brand_color[b], 
                   text = b, text.vjust = -1, niceFacing = TRUE)
}

circos.clear()
