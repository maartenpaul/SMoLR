\name{rotate_coord}
\alias{rotate_coord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rotate Cartesian coordinates
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Rotate Cartesian Coordinates around a given center. Translate or flip cartesian coordinates. Stretch cartesian coordinates.
}
\usage{
rotate_coord(x, y, angle, type = c("degrees", "radial"), method = c("transform", "polar", "polar_extended"), center = c(0, 0), translate = NULL, stretch = NULL, flip = FALSE)
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
  \item{angle}{
%%     ~~Describe \code{angle} here~~
Angle the points should be rotated. Positive angles rotate clockwise, negative counterclock.
}
  \item{type}{
%%     ~~Describe \code{type} here~~
Define the type of angle that is given. Default is "degrees", alternative is "radial"
}
  \item{method}{
%%     ~~Describe \code{method} here~~
Define method to be used for rotation. Transform uses an transformation matrix (fast), polar transforms coordinates to polar (slow), polar_extended additionally returns distances of points from center. 
}
  \item{center}{
%%     ~~Describe \code{center} here~~
Center around which coordinates should be rotated, give a vector containing and x and y coordinate.
}
  \item{translate}{
%%     ~~Describe \code{translate} here~~
Move coordinates in x and y direction, give a vector containing x and y coordinate
}
  \item{stretch}{
%%     ~~Describe \code{stretch} here~~
Stretch or shrink coordinates around the center by a given factor (i.e. 1.5 stretch 50 procent, 0.5 shrink 50%)
}
  \item{flip}{
%%     ~~Describe \code{flip} here~~
Flip coordinates around the x axis at the center
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
Returns rotated coordinates
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
rotate_coord(c(25,50,100,150),c(150,125,50,25),angle=1.2,type="radial",method="polar_extended")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

