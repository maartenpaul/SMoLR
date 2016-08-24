\name{SMOLR_PLOT}
\alias{SMOLR_PLOT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plots single molecule datasets 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SMOLR_PLOT(x, y, size, color, rev.size, rev.color, input_xsize, input_ysize, px, grey, split_ch, fit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{color}{
%%     ~~Describe \code{color} here~~
}
  \item{rev.size}{
%%     ~~Describe \code{rev.size} here~~
}
  \item{rev.color}{
%%     ~~Describe \code{rev.color} here~~
}
  \item{input_xsize}{
%%     ~~Describe \code{input_xsize} here~~
}
  \item{input_ysize}{
%%     ~~Describe \code{input_ysize} here~~
}
  \item{px}{
%%     ~~Describe \code{px} here~~
}
  \item{grey}{
%%     ~~Describe \code{grey} here~~
}
  \item{split_ch}{
%%     ~~Describe \code{split_ch} here~~
}
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
  \item{color_scale}{
Add your own color scale to the plot. The values in color are automatically scaled between the first and last color in the scale. Using adjustcolor() transparency can be added to the color scale.
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
SMOLR_PLOT(smolr,rev.color=TRUE,size="off",split_ch=TRUE)

SMOLR_PLOT(smolrdata,color_scale = adjustcolor(c("red","green","magenta"),0.3))
}

