utils::globalVariables(c(".", "N", "V1", "word1", "word2", "word3", "word4"))

wordpred <-
function(ngram) {
    start.time <- Sys.time()

    textParts <- wordpred_tokenize(ngram)

    keep <- !grepl("EOS", textParts, fixed=TRUE)

    if(length(keep) > 0) {
      # state == 0: searching for EOS (end of sentence) at the final positions
      # state == 1: searching for EOS after good text
      # state == 2: remaining words are to be removed
      state <- 0
      count <- 0
      for(i in length(keep):1) {
        if(state==0 && !keep[i]) {
          state <- 2
        } else if(state==0 && keep[i]) {
          state <- 1
          count <- count+1
        } else if(state==1 && keep[i] && count<(wordpred.data.ngrams-1)) {
          count <- count+1
        } else if(state==1) {
          state <- 2
          keep[i] <- FALSE
        } else if(state==2) {
          keep[i] <- FALSE
        }
      }
    }    

    textParts <- textParts[keep]
    if(length(textParts) < 3) {
      textParts <- c("SOS", textParts)
    }

    res <- c()
    savedTextParts <- textParts

    ngramsUsed <- rep(0, wordpred.data.ngrams-1+1)
    names(ngramsUsed) <- 0:(wordpred.data.ngrams-1)    

    if(length(textParts) == 3) {
      res <- wordpred.data[word1==textParts[1] & word2==textParts[2] & word3==textParts[3], sum(N), by=.(word1, word2, word3, word4)][order(-V1), head(word4, 6)]
      textParts <- tail(textParts, 2)
      ngramsUsed["3"] <- ngramsUsed["3"] + length(res)
    }
    
    if(length(res)<6 && length(textParts)==2) {
      res2 <- wordpred.data[word2==textParts[1] & word3==textParts[2], sum(N), by=.(word2, word3, word4)][order(-V1), word4]
      res2 <- head(setdiff(res2, res), 6-length(res))
      ngramsUsed["2"] <- ngramsUsed["2"] + length(res2)
      res <- c(res, res2)
      textParts <- tail(textParts, 1)
    }

    if(length(res)<6 && length(textParts)==1) {
      res2 <- wordpred.data[word3==textParts[1], sum(N), by=.(word3, word4)][order(-V1), word4]
      res2 <- head(setdiff(res2, res), 6-length(res))
      ngramsUsed["1"] <- ngramsUsed["1"] + length(res2)
      res <- c(res, res2)
    }
    
    if(length(res)<6) {
      res2 <- wordpred.data[, sum(N), by=.(word4)][order(-V1), word4]
      res2 <- head(setdiff(res2, res), 6-length(res))
      attr(res, "ngramsUsed")[0] <- attr(res, "ngramsUsed")[0] + length(res2)
      ngramsUsed["0"] <- ngramsUsed["0"] + length(res2)
      res <- c(res, res2)
    }    

    end.time <- Sys.time()

    attr(res, "ngramsUsed") <- ngramsUsed
    attr(res, "ngram") <- savedTextParts
    attr(res, "time") <- end.time - start.time
    res
}
