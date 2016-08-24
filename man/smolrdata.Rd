\name{smolrdata}
\alias{smolrdata}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Test data for single moleculer 
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A single molecule data set, representing data from 3 Channels with random precisions.
}
\usage{data("smolrdata")}
\format{
  A data frame with 693 observations on the following 4 variables.
  \describe{
    \item{\code{X}}{X coordinates}
    \item{\code{Y}}{Y coordinates}
    \item{\code{Precision}}{Precision}
    \item{\code{Channel}}{Channel number}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Optical Imaging Center ErasmusMC Rotterdam
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{

plot(SMOLR(smolrdata))
plot(SMOLR_KDE(smolrdata))
SMOLR_PLOT(smolrdata) 
SMOLR_PARAMETER(smolrdata)
}
\keyword{datasets}
