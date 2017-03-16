wordpred_tokenize <-
function(str) {
    str <- wordpred_prepare(str)
    str <- gsub("\\.|!|\\?", " EOS ", str)
    str <- gsub("\\s+", " ", str)  
    words <- unlist(strsplit(str, " "))
    words <- wordpred_clean(words)
    words
}
