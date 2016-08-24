\name{SMOLR_PARAMETER}
\alias{SMOLR_PARAMETER}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get parameters from a single molecule dataset
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SMOLR_PARAMETER(x, y, ch, prec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
X coordinates
}
  \item{y}{
%%     ~~Describe \code{y} here~~
Y coordinates
}
  \item{ch}{
%%     ~~Describe \code{ch} here~~
Channel numbers
}
  \item{prec}{
%%     ~~Describe \code{prec} here~~
Precision
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
SMOLR_PARAMETER(c(400,400,320),c(200,120,190),ch=c(1,2,2),prec=c(20,21,15))
SMOLR_PARAMETER(smlmrdata)
}

