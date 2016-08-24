\name{SMOLR_DBSCAN}
\alias{SMOLR_DBSCAN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Analysis of SMOLR localization data by DBSCAN clustering
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SMOLR_DBSCAN(x, y, ch, prec, eps, MinPts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{ch}{
%%     ~~Describe \code{ch} here~~
}
  \item{prec}{
%%     ~~Describe \code{prec} here~~
}
  \item{eps}{
%%     ~~Describe \code{eps} here~~
}
  \item{MinPts}{
%%     ~~Describe \code{MinPts} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, ch, prec, eps, MinPts) 
{
    UseMethod("SMOLR_DBSCAN")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
