\name{SMOLR_POINT_FEATURES}
\alias{SMOLR_POINT_FEATURES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
SMOLR POINT FEATURES
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Extraction of features based on the localization data
}
\usage{
SMOLR_POINT_FEATURES(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
item{x}{
localization data, SMOLR_DBSCAN or SMOLR_KDE object, can be a list of objects
}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{comp1 }{Description of 'comp1'}
\item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Optical Imaging Centre ErasmusMC Rotterdam
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~

}
\examples{
#obtain statistics for a localization data.frame
x1<-SMOLR_POINT_FEATURES(smolrdata)
x1
#obtain statistics after clustering using SMOLR_KDE
x2<-SMOLR_POINT_FEATURES(SMOLR_KDE(smolrdata))
x2
#obtain statistics after clustering using SMOLR_DBSCAN
x3<-SMOLR_POINT_FEATURES(SMOLR_DBSCAN(smolrdata,MinPts=10))
x3  
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
