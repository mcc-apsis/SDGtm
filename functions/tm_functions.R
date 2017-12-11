get_tokensIterator <- function(i_data, ignorewords=c(""), verbose=FALSE) {
  
  if (verbose) t1 = Sys.time()
  # Create iterator over tokens
  tokens <- space_tokenizer(i_data$text) %>% 
    lapply(., function(x) {
      x <- x[which(!x %in% c(tm::stopwords(), ignorewords))] %>% 
        tm::PlainTextDocument() %>% 
        tm::removeNumbers() %>% 
        tm::removePunctuation() %>% 
        tm::stemDocument()
      
      out <- x$content
      
      out <- out[which(nchar(out) >= 3)]
      
      return(out)
    })
  
  it <- itoken(tokens, ids=i_data$doc, progressbar = FALSE)
  if (verbose) cat(paste0("Create tokens iterator: ", round(as.numeric(difftime(Sys.time(), t1, units = 'sec')), digits=2), " secs\n"))
  return(it)
}

create_corpora <- function(i_it, verbose=FALSE) {
  # Create vocabulary. Terms will be unigrams (simple words).
  if (verbose) t1 = Sys.time()
  corpus <- create_corpus(i_it)
  
  if (verbose) cat(paste0("Create corpus: ", round(as.numeric(difftime(Sys.time(), t1, units = 'sec')), digits=2), " secs\n"))
  
  return(corpus)
}

create_vocab <- function(i_it, verbose=FALSE, ...) {
  # Create vocabulary. Terms will be unigrams (simple words).
  if (verbose) t1 = Sys.time()
  vocab <- create_vocabulary(i_it)
  
  # Prune vocabulary
  vocab <- prune_vocabulary(vocab, ...)
  if (verbose) cat(paste0("Create vocabulary: ", round(as.numeric(difftime(Sys.time(), t1, units = 'sec')), digits=2), " secs\n"))
  
  return(vocab)
}

generate_dtm <- function(i_vocab, i_it, i_data, verbose=FALSE) {
  if (verbose) t1 = Sys.time()
  vectorizer <- vocab_vectorizer(i_vocab)
  
  dtm <- create_dtm(i_it, vectorizer)
  
  if (verbose) cat(paste0("  > DTM dimensions (initial)     : ", paste(dim(dtm), collapse="x"), "\n"))
  
  tfidf <- TfIdf$new(norm="l2")
  
  dtm_tfidf <- tfidf$fit_transform(dtm)
  
  if (verbose) cat(paste0("  > DTM dimensions (after TF-IDF): ", paste(dim(dtm_tfidf), collapse="x"), "\n"))
  
  if (verbose) cat(paste0("  > Are they identical? ", identical(dtm, dtm_tfidf), "\n"))
  
  #row.names(dtm_tfidf) <- i_data$UT
  
  if (verbose) cat(paste0("Generate DTM: ", round(as.numeric(difftime(Sys.time(), t1, units = 'sec')), digits=2), " secs\n"))
  return(dtm_tfidf)
}

compute_cosine_similarity <- function(i_vocab, i_it, verbose=FALSE) {
  
  # Create term-co-occurence matrix
  if (verbose) t1 = Sys.time()
  # Use our filtered vocabulary
  vectorizer <- vocab_vectorizer(i_vocab,
                                 # don't vectorize input
                                 grow_dtm = FALSE,
                                 # use window of 5 for context words
                                 skip_grams_window = 10L)
  
  # Create term-co-occurence matrix
  tcm <- create_tcm(i_it, vectorizer)
  if (verbose) cat(paste0("Create TCM: ", round(as.numeric(difftime(Sys.time(), t1, units = 'sec')), digits=2), " secs\n"))
  
  
  # Factorize TCM via GloVe
  if (verbose) t1 = Sys.time()
  glove <- GlobalVectors$new(word_vectors_size = 50, vocabulary = i_vocab, x_max = 10)
  fit(tcm, glove, n_iter = 20)
  if (verbose) cat(paste0("Factorise TCM: ", round(as.numeric(difftime(Sys.time(), t1, units = 'sec')), digits=2), " secs\n"))
  
  word_vectors <- glove$get_word_vectors()
  
  # Compute cosine similarity matrix
  if (verbose) t1 = Sys.time()
  cos_sim <- sim2(x = word_vectors, method = "cosine", norm = "l2")
  if (verbose) cat(paste0("Compute cosine similarity matrix: ", round(as.numeric(difftime(Sys.time(), t1, units = 'sec')), digits=2), " secs\n"))
  
  return(cos_sim)
}

compute_optim_topic_model <- function(i_dtm, i_cossim, i_topicrange, i_nbterms=50, verbose=FALSE) {
  tm  <- list()
  coh <- list()
  
  #library(doParallel)
  
  for (k in 1:length(i_topicrange)) {
    if (verbose) cat(paste0("Processing window topic model ", k, " with ", i_topicrange[k], " topics...\n"))
    if (verbose) t1 = Sys.time()
    if (verbose) cat(paste0("  - Factorising DTM...\n"))
    tm[[k]] <- nmf(as.matrix(i_dtm), rank = i_topicrange[k], seed="nndsvd")
    
    # Compute topics coherence
    if (verbose) cat(paste0("  - Computing topic coherence...\n"))
    coh[[k]] <- list()
    coh[[k]]$topic_coherence <- unlist(lapply(1:i_topicrange[k], function(x) {
      th <- names(sort(tm[[k]]@fit@H[x,], decreasing = T))[1:i_nbterms] # Take first N terms of the topic
      
      cs <- i_cossim[th, th]
      diag(cs) <- 0.0
      cs[which(lower.tri(cs), arr.ind = TRUE)] <- 0.0
      
      coh <- 1/choose(i_nbterms, 2)*sum(cs)
      
    }))
    
    coh[[k]]$topicmodel_coherence <- 1/i_topicrange[k]*sum(coh[[k]]$topic_coherence)
    
    if (verbose) cat(paste0("    -> Topic model coherence: ", coh[[k]]$topicmodel_coherence, "\n"))
    
    if (verbose) cat(paste0("Topic model ", k, " took: ", round(as.numeric(difftime(Sys.time(), t1, units = 'sec')), digits=2), " secs\n"))
  }
  
  cohs <- unlist(lapply(coh, function(x) x$topicmodel_coherence))
  optim_k <- which(cohs == max(cohs)[1])
  
  if (verbose) cat(paste0("Optimal window topic model has ", i_topicrange[optim_k]," topics (coh=", round(cohs[optim_k], digits=2), ")\n"))
  
  return(tm[[optim_k]])
}