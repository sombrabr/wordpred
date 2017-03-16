wordpred_clean <-
function(ngrams, prepare=FALSE) {
    if(prepare) ngrams <- wordpred_prepare(ngrams)
    ngrams <- gsub("(?!')[[:punct:]]", "", ngrams, perl=TRUE)
    ngrams <- grep("^[a-zA-Z']{1,}$", ngrams, value=TRUE)
    ngrams
}
