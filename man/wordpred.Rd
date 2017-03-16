\name{wordpred}
\alias{wordpred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Word Prediction
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function returns 6 words that would be possible to continue the phrase.
}
\usage{
wordpred(ngram)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ngram}{
%%     ~~Describe \code{ngram} here~~
A phrase to be tokenized or a vector of strings.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The \code{ngram} will be tokenized and cleaned on the process of predicting the
possible next words.

The cleaning process does:

\itemize{
  \item translation of any variation of the apostrophe (\code{\'}) to a simple one
  \item convert the text to UTF-8
  \item lower case the text
  \item remove punctuation
  \item remove words with numbers
  \item remove spaces
}

Only the last 3 words, after the cleaning process, are used in the prediction 
process.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A vector with 6 words that are possibilities to continue the phrase.

The vector has 2 attributes:

\itemize{
  \item ngramsUsed: a list showing, from the total index lookup, how many words were used in each lookup
  \item ngram: the words used to search in the index
  \item time: time spent on prediction
}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
%%  ~~who you are~~
Eduardo Bortoluzzi Jr, Cloud Architect, data science student
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{

pred1 <- wordpred("I went to the store. There I bought a case of")
sum(pred1=="beer") > 0
str(pred1)

pred2 <- wordpred(c("a", "case", "of"))
sum(pred2=="beer") > 0
str(pred2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\keyword{package}
