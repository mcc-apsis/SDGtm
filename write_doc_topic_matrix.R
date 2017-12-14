#install.packages("xlsx")
library(xlsx)

map_names <- data.frame(xml2pdf=substr(basename(row.names(dtm$dtm)), 1, nchar(basename(row.names(dtm$dtm)))-4)) %>% 
  inner_join(v_pdf_years, by=c("xml2pdf"="filename")) %>% mutate(label=paste0(author, " (", year, ")"))

dtt <- tm_sdg_20171213[["30"]]@gamma %>% 
  as.data.frame()

names(dtt) <- paste("Topic", 1:30)
dups <- map_names$label[which(duplicated(map_names$label))]
  
map_names$label[which(duplicated(map_names$label))] <- paste0(substr(dups, 1, nchar(dups)-1), "b)")
map_names$label[which(map_names$label %in% dups)]   <- paste0(substr(map_names$label[which(map_names$label %in% dups)], 1, nchar(map_names$label[which(map_names$label %in% dups)])-1), "a)")

dups <- map_names$label[which(duplicated(map_names$label))]
map_names$label[which(duplicated(map_names$label))] <- paste0(substr(dups, 1, nchar(dups)-1), "c)")

row.names(dtt) <- map_names$label

nrows <- dim(dtt)[1]
ncols <- dim(dtt)[2]

wb     <- createWorkbook()
sheet  <- createSheet(wb, "Sheet1")

addDataFrame(dtt, sheet)


# cs <- CellStyle(wb) +
#   Fill(backgroundColor="orange", foregroundColor="orange",
#        pattern="SOLID_FOREGROUND") 
# 
# setCellStyle(cell.1, cellStyle1) 

# Then save the workbook 
saveWorkbook(wb, "DocTopicTable_30topics.xlsx")
