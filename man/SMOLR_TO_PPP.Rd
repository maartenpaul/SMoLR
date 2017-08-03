\name{SMOLR_TO_PPP}
\alias{SMOLR_TO_PPP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Convert localizations to point pattern for spatstat package
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A function to convert localization data coordinates to point patterns for the spatstat package. Input can be separate vectors, data.frame or a list of data.frames.
}
\usage{
SMOLR_TO_PPP(x, y, xlim, ylim, marks, shape)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
X coordinates in nm, or a data.frame or list of data.frames with localization data.
}
  \item{y}{
Y coordinates in nm
}
  \item{xlim}{
Vector with the upper and lower limit of x coordinates 
}
  \item{ylim}{
Vector with the upper and lower limit of y coordinates 
}
  \item{marks}{
Vector to indictate different types/channels of the coordinates. If list or data.frame is used as input the channel column will be used for the marks.
}
  \item{shape}{
Shape of the point pattern, either a rectangle ("rect") or circle ("circle"). Default is "rect". 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a spatstat ppp object, or a list of ppp objects.
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
\code{\link{ppp}}
}
\examples{
example <- SMOLR_TO_PPP(smolrdata)
plot(example)
## The function is currently defined as
function (x, y, xlim, ylim, marks, shape) 
{
    UseMethod("smlmr_to_ppp")
  }


#make triangulation plot
x <- SMOLR_TO_PPP(smolrdata)
#channel 1
plot(dirichlet(x[x$marks==1]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Point Pattern }
\keyword{ ~spatstat }% __ONLY ONE__ keyword per line
