wordpred_prepare <-
function(data) {
    data <- gsub("\u2019|\u2018|\u201b|\u2032|\u2035", "'", data)
    data <- iconv(enc2utf8(data), sub = "byte")
    data <- tolower(data)
    data
}
