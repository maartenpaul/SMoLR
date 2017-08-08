\name{plot.smolr_image}
\alias{plot.smolr_image}
\alias{plot.smolr_kde}
\alias{plot.smolr_dbscan}
\alias{plot_lut.smolr_image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PLOT of SMOLR objects
}
\description{
Different objects from SMoLR can be plotted using the \code{plot()} function. Depending on the type of object an image or a scatter plot will be returned.
}
\usage{
plot.smolr_image(x, y, saturate = 0, brightness = 0, contrast = 1, rgb = F, ...)
plot.smolr_kde(x, y, saturate = 0, brightness = 0, contrast = 1, rgb = F, ...)
plot_lut.smolr_image(channel,lut)
plot.smolr_dbscan(x,hide_noise=FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saturate}{
Fraction of pixels that will be saturated in the plotted image
}
  \item{brightness}{
Adjust brightness, value between 0 and 1, this will shift the pixel values with the indicated value (default=0).
}
  \item{contrast}{
Adjust contrast, pixel values are multiplied with the input value (default = 1).
}
  \item{rgb}{
Make an RGB image, default=FALSE
}
  \item{lut}{
Use a lookup table from the pals package
}
  \item{hide_noise}{
  Used for a smolr_dbscan, this will hide to localizations which are not in a cluster (background/noise).
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
Optical Imaging Centre ErasmusMC Rotterdam
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{pals}},\code{\link{continuous}}
}
\examples{
plot(SMOLR(smolrdata))
plot(SMOLR(smolrdata),brightness = 0.9)
plot(SMOLR(smolrdata),saturate = 0.2)

plot(SMOLR_KDE(smolrdata))
plot(SMOLR_KDE(smolrdata),contrast=2)

plot(SMOLR_DBSCAN(smolrdata,MinPts=15),hide_noise = F)
plot(SMOLR_DBSCAN(smolrdata,MinPts=15),hide_noise = T)

plot_lut(SMOLR(smolrdata),channel = 1)
plot_lut(SMOLR(smolrdata),channel = 2,"warmcool")
plot_lut(SMOLR(smolrdata),channel = 3,"coolwarm")
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
