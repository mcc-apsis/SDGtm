suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringdist))
suppressMessages(library(knitr))

Nterms  <- 10

get_TMids <- function(i_tm, i_ntopics, i_nterms=3) {
  
  termstopics <- row.names(posterior(i_tm)$terms %>% as.matrix() %>% t())
  
  out <- unlist(lapply(1:i_ntopics, 
                       function(x) {
                         paste0(x, ": ", paste(termstopics[rev(order((posterior(i_tm)$terms %>% 
                                                                        as.matrix() %>% 
                                                                        t() %>% 
                                                                        as.data.frame())[,x]))][1:i_nterms], 
                                               collapse=", "))
                       }))
  
  return(out)
}

get_TMterms <- function(i_tm, i_ntopics, i_nterms) {
  
  termstopics <- row.names(posterior(i_tm)$terms %>% as.matrix() %>% t())
  
  out <- lapply(1:i_ntopics, 
                function(x) {
                  termstopics[rev(order((posterior(i_tm)$terms %>% 
                                           as.matrix() %>% 
                                           t() %>% as.data.frame())[,x]))][1:i_nterms]
                })
  
  return(out)
}


compute_editDistance <- function(v1, v2, SORT=TRUE, NORMALISE=FALSE) {
  
  symbols <- c(letters,
               LETTERS,
               paste(0:9),
               " ", ",", ";", ".", ":", "-", "_", "!", "§", "$", "%", "&", "/", "(", ")", "=", "?", "`", "´", "*", "+", "~", "'", "#", "<", ">", "|", "°", "^")
  
  # Sort vectors is necessary
  if (SORT) {
    v1 <- sort(v1)
    v2 <- sort(v2)
  }
  
  # Generate mapping between splitting vars and associated values and letters
  map_svarval_char <- data.frame(
    svarval = sort(unique(c(v1, v2))),
    char    = symbols[1:length(unique(c(v1, v2)))],
    stringsAsFactors = FALSE)
  
  # Replace splitting vars and associated values by letters
  out <- stringdist(
    paste0(sapply(v1, function(x) map_svarval_char$char[which(map_svarval_char$svarval == x)]), collapse=""),
    paste0(sapply(v2, function(x) map_svarval_char$char[which(map_svarval_char$svarval == x)]), collapse="")
  )
  
  if (NORMALISE) out <- out/max(c(length(v1), length(v2)))
  
  return(out)
}

compute_simpleDistance <- function(v1, v2, SORT=FALSE, NORMALISE=FALSE) {
  
  symbols <- c(letters,
               LETTERS,
               paste(0:9),
               " ", ",", ";", ".", ":", "-", "_", "!", "§", "$", "%", "&", "/", "(", ")", "=", "?", "`", "´", "*", "+", "~", "'", "#", "<", ">", "|", "°", "^")
  
  # Sort vectors is necessary
  if (SORT) {
    v1 <- sort(v1)
    v2 <- sort(v2)
  }
  
  # Distance #1
  # out <- length(which(v1 %in% v2))
  
  # Distance #2
  # out <- sum(rev(seq(1:length(v1)))[which(v1 %in% v2)])
  
  # Distance #3
  out <- sum(rev(seq(1:length(v1)))[as.numeric(unlist(sapply(v1, function(x) which(x == v2))))])
  
  if (NORMALISE) out <- out/max(c(length(v1), length(v2)))
  
  return(out)
}

tm_list <- tm_sdg_gsAbs_20180105

# Generate summary table
for (k_tmid in 1:(length(tm_list)-1)) {
  
  ktm1_id <- order(as.numeric(names(tm_sdg_gsAbs_20180105)))[k_tmid]
  ktm2_id <- order(as.numeric(names(tm_sdg_gsAbs_20180105)))[k_tmid+1]
  
  k_tm1 <- names(tm_list)[ktm1_id]
  k_tm2 <- names(tm_list)[ktm2_id]
  
  ntm1 <- as.numeric(k_tm1)
  ntm2 <- as.numeric(k_tm2)
  
  TM1_terms <- get_TMterms(tm_list[[k_tm1]], ntm1, Nterms)
  TM2_terms <- get_TMterms(tm_list[[k_tm2]], ntm2, Nterms)
  
  m = matrix(NA, nrow = ntm1, ncol=ntm2)
  
  for (kr in 1:ntm1) {
    for (kc in 1:ntm2) {
      m[kr,kc] <- compute_simpleDistance(TM1_terms[[kr]], TM2_terms[[kc]], SORT = FALSE)
    }
  }
  
  #m <- abs(m - 10)
  
  #m[which(m == Nterms)] <- NA
  df <- m %>% 
    as.data.frame()
  
  TM1_ids <- get_TMids(tm_list[[k_tm1]], ntm1)
  TM2_ids <- get_TMids(tm_list[[k_tm2]], ntm2)
  
  names(df) <- get_TMids(tm_list[[k_tm2]], ntm2)
  df <- df %>% 
    mutate(tm1_id = factor(TM1_ids, levels=TM1_ids, ordered=TRUE)) %>% 
    gather(tm2_id,value, -tm1_id) %>% 
    mutate(tm2_id = factor(tm2_id, levels=TM2_ids, ordered=TRUE)) %>% 
    mutate(label = paste(value))
  
  p = ggplot(df) +
    geom_tile(aes(x=tm2_id, y=tm1_id, fill=value)) +
    geom_text(aes(x=tm2_id, y=tm1_id, label=label), df %>% filter(value == 0), color="white") +
    geom_text(aes(x=tm2_id, y=tm1_id, label=label), df %>% filter(value != 0), color="#bbbbbb") +
    theme_bw() +
    scale_fill_gradient(low="#ffffff", high="#ff0000") +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) + 
    xlab("") + ylab("")
  print(p)
  
  # Best matching topic (Look across colums for single match)
  tm2_match <- data.frame(
    tm2id  = TM2_ids,
    stable = rep(FALSE, length(TM2_ids)),
    merged = rep(FALSE, length(TM2_ids)),
    #split  = rep(FALSE, length(TM2_ids)),
    new    = rep(FALSE, length(TM2_ids))
  )
  
  for (kc in 1:ntm2) {
    
    outliers <- boxplot(m[,kc], plot = FALSE)$out
    
    if (length(outliers) == 0) {
      tm2_match$new[which(tm2_match$tm2id == TM2_ids[kc])] <- TRUE
    }
    
    if (length(outliers) == 1) {
      tm2_match$stable[which(tm2_match$tm2id == TM2_ids[kc])] <- TRUE
    }
    
    if (length(outliers) > 1) {
      out_max    <- max(outliers)
      out_2ndmax <-max(outliers[which(outliers != max(outliers))])
      if (abs(out_max - out_2ndmax) < 5) {
        tm2_match$merged[which(tm2_match$tm2id == TM2_ids[kc])] <- TRUE
      } else {
        tm2_match$stable[which(tm2_match$tm2id == TM2_ids[kc])] <- TRUE
      }
    }
  }
  
  
  
  
  print("The following topics are stable:")  
    ids <- which(m[,kc] != 0)
    if (length(ids) == 1) {
      print(paste0("  - ", TM2_ids[kc], " (score: ", m[ids,kc]," - former topic: ", TM1_ids[ids], ")"))
      tm2_match$stable[which(tm2_match$tm2id == TM2_ids[kc])] <- TRUE
    }
  }
  
  # # Best matching topic (Look across colums for single match)
  # tm2_match <- data.frame(
  #   tm2id  = TM2_ids,
  #   stable = rep(FALSE, length(TM2_ids)),
  #   merged = rep(FALSE, length(TM2_ids)),
  #   split  = rep(FALSE, length(TM2_ids)),
  #   new    = rep(FALSE, length(TM2_ids))
  # )
  # print("The following topics are stable:")
  # for (kc in 1:ntm2) {
  #   ids <- which(m[,kc] != 0)
  #   if (length(ids) == 1) {
  #     print(paste0("  - ", TM2_ids[kc], " (score: ", m[ids,kc]," - former topic: ", TM1_ids[ids], ")"))
  #     tm2_match$stable[which(tm2_match$tm2id == TM2_ids[kc])] <- TRUE
  #   }
  # }
  # 
  # # Merged topics (Look across columns)
  # print("The following topics have been merged:")
  # for (kc in 1:ntm2) {
  #   ids <- which(m[,kc] != 0)
  #   if (length(ids) > 1) {
  #     print(paste0("  - ", TM2_ids[kc], " (former topics: ", paste(TM1_ids[ids], collapse="; "), ")"))
  #     tm2_match$merged[which(tm2_match$tm2id == TM2_ids[kc])] <- TRUE
  #   }
  # }
  # 
  # # Split topics (Look across rows)
  # print("The following topics have been split:")
  # for (kr in 1:ntm1) {
  #   ids <- which(m[kr,] != 0)
  #   if (length(ids) > 1) {
  #     print(paste0("  - ", TM1_ids[kr], " (new topics: ", paste(TM2_ids[ids], collapse="; "), ")"))
  #     for (kc in ids) {
  #       tm2_match$split[which(tm2_match$tm2id == TM2_ids[kc])] <- TRUE
  #     }
  #   }
  # }
  # 
  # 
  # # New topics (Look across columns where sum over rows == 0)
  # print("The following topics are new:")
  # for (kc in 1:ntm2) {
  #   ids <- which(m[,kc] != 0)
  #   if (length(ids) == 0) {
  #     print(paste0("  - ", TM2_ids[kc]))
  #     tm2_match$new[which(tm2_match$tm2id == TM2_ids[kc])] <- TRUE
  #   }
  # }