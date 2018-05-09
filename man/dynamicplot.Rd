\name{dynamicplot}
\alias{dynamicplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dynamic Shiny plot for SMoLR}
\description{
This function allows to run a Shiny web application to explore the localization data
}
\usage{
dynamicplot(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
the localization data of a single image to be viewed in the dynamic Shiny plot
}
  \item{dbdata}{
  dbscan clustering object of the localizations in the \code{var}, when ommitted this is calculated in the dynamic plot while loading, but this might increase the time to load application
  }
  \item{overlay}{
a \code{SMOLR_image} object of the localizations in the \code{var}, when ommitted this is calculated in the dynamic plot while loading, but this might increase the time to load application  }
}
\details{
In the shiny application one can drag on the plot to select clusters with certain values, those will be color codes in the plot on the right. Additionally it is possible to zoom-in in the image, by dragging in the image, the plot and image will be shown for the selected zoom-in area.
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
dynamicplot(smolrdata[smolrdata$Channel==1,],eps = 50,MinPts=5)
dynamicplot(smolrdata[smolrdata$Channel==3,],eps = 50,MinPts=5)

download.file("https://github.com/maartenpaul/SMoLR_data/raw/master/dynamicplot_data.zip","dynamicplot_data.zip")
unzip("dynamicplot_data.zip")

load("dynamicplot_data.Rdata")
dynamicplot(locdata)

#preload the data
dbdata <- SMOLR_DBSCAN(locdata)
overlay <- SMOLR(locdata)
dynamicplot(locdata,dbdat=dbdata,overlay=overlay)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
