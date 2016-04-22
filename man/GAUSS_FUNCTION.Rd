\name{GAUSS_FUNCTION}
\alias{GAUSS_FUNCTION}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Make a 1D or 2D gauss curve
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Makes a 1D or 2D gauss curve 
}
\usage{
GAUSS_FUNCTION(sigx, sigy = NULL, xo = 0, yo = 0, one_D = FALSE, int_norm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sigx}{
%%     ~~Describe \code{sigx} here~~
sigma in the x direction
}
  \item{sigy}{
%%     ~~Describe \code{sigy} here~~
sigma in the y direction
}
  \item{xo}{
%%     ~~Describe \code{xo} here~~
offset in the x direction
}
  \item{yo}{  
%%     ~~Describe \code{yo} here~~
offset in the y direction
}
  \item{one_D}{
%%     ~~Describe \code{one_D} here~~
If one_D is TRUE a one dimesional gauss curve is returned.  
}
  \item{int_norm}{  
%%     ~~Describe \code{int_norm} here~~
Normalizes the integrated gauss (keeps equal erea under the curve)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Makes the 2D gauss curve that is used in the SMLMR function
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a matrix with a 2D gauss curve
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
GAUSS_FUNCTION(3)
GAUSS_FUNCTION(sigx=3,sigy=6,xo=0.5,yo=0.6)
}



