\name{plot.smolr_image}
\alias{plot.smolr_image}
\alias{plot_lut.smolr_image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PLOT of SMOLR image
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.smolr_image(x, y, saturate = 0, brightness = 0, contrast = 1, rgb = F, ...)
plot_lut.smolr_image(channel,lut)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saturate}{
Fraction of pixels that will be saturated in the plotted image
}
  \item{brightness}{
%%     ~~Describe \code{brightness} here~~
}
  \item{contrast}{
%%     ~~Describe \code{contrast} here~~
}
  \item{rgb}{
Make an RGB image, default=FALSE
}
  \item{lut}{
Use a lookup table from the pals package
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
Erasmus MC, Johan Slotman and Maarten Paul
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{pals}},\code{\link{continous}}
}
\examples{
plot(SMOLR(smolrdata))
plot(SMOLR(smolrdata),brightness = 0.9)
plot(SMOLR(smolrdata),saturate = 0.2)

plot_lut(SMOLR(smolrdata),channel = 2)
plot_lut(SMOLR(smolrdata),channel = 3,)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
