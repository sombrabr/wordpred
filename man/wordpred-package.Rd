\name{wordpred-package}
\alias{wordpred-package}
\docType{package}
\title{
\packageTitle{wordpred}
}
\description{
\packageDescription{wordpred}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{wordpred}
\packageIndices{wordpred}
%%~~ An overview of how to use the package, including the most important ~~
%%~~ functions ~~
The main function is \code{\link{wordpred}}. Just call it passing a sentence.
}
\author{
\packageAuthor{wordpred}

Maintainer: \packageMaintainer{wordpred}
}
%%\references{
%%~~ Literature or other references for background information ~~
%%}
\keyword{ package }
\seealso{
%%~~ Optional links to other man pages, e.g. ~~
%%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
\code{\link{wordpred}}
}
\examples{
wordpred("I went to the store. There I bought a case of")

wordpred(c("I", "went", "to", "the", "store.", "There", "I", "bought", "a", "case", "of" ))
}
