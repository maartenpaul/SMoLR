\name{SMOLR_POINT_FEATURES}
\alias{SMOLR_POINT_FEATURES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
SMOLR COORDINATE-BASED FEATURES
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Extraction of features based on the coordinates of the localization data
}
\usage{
SMOLR_POINT_FEATURES(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
item{x}{
localization data, \code{SMOLR_DBSCAN} or \code{SMOLR_KDE} object, can be a list of objects
}
item{skeleton}{
estimate the skeleton length, using skeltonization of a binary image.
}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{data.frame} with columns indicating the different obtained values
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
}