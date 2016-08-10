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
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{marks}{
%%     ~~Describe \code{marks} here~~
}
  \item{shape}{
%%     ~~Describe \code{shape} here~~
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
