\name{rotate3d}
\alias{rotate3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Rotate 3D 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Rotate 3d coordinates around given angles
}
\usage{
rotate3d(x = NULL, y = NULL, z = NULL, i = 0, j = 0, k = 0)
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
  \item{z}{
%%     ~~Describe \code{z} here~~
Z coordinates
}
  \item{i}{
%%     ~~Describe \code{i} here~~
Angle to rotate around in the xy plane
}
  \item{j}{
%%     ~~Describe \code{j} here~~
Angle to rotate around in the xz plane
}
  \item{k}{
%%     ~~Describe \code{k} here~~
Angle to rotate around in the yz plane
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Rotates x,y,z coordinates in three planes around the 0,0 center.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns rotated x,y,z coordinates 
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
rotate3d(c(100,125,175),c(175,125,100),c(150,100,100), 12,50,45)
}

