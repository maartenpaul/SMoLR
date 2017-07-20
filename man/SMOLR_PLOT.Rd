\name{SMOLR_PLOT}
\alias{SMOLR_PLOT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plots single molecule datasets in a scatter plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SMOLR_PLOT(x,y,size,color,rev.size,rev.color, xlim, ylim,px,grey,split_ch,fit,clim,slim,alpha,overlay,contrast,color_scale)
\method{SMOLR_PLOT}{data.frame}SMOLR_PLOT(x,size,color,rev.size,rev.color, xlim, ylim,px,grey,split_ch,fit,clim,slim,alpha,overlay,contrast,color_scale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
Vector of x values (nm) or data.frame containing x and y collumns
}
  \item{y}{
%%     ~~Describe \code{y} here~~
Vector of y values (nm)
}
  \item{size}{
%%     ~~Describe \code{size} here~~
Vector of variables that will be indicated by the size of the plotted points
}
  \item{color}{
%%     ~~Describe \code{color} here~~
Vector of variables that will be indicated by the color of the plotted points
}
  \item{rev.size}{
%%     ~~Describe \code{rev.size} here~~
If FALSE small numbers will give rise to smaller points, if TRUE the higher the number the smaller the size.
}
  \item{rev.color}{
%%     ~~Describe \code{rev.color} here~~
If FALSE colors will run from red to green, if TRUE colors will run from green to red
}
  \item{xlim}{
%%     ~~Describe \code{input_xsize} here~~
Vector containing the lower and upper limit for x values displayed
}
  \item{ylim}{
%%     ~~Describe \code{input_ysize} here~~
Vector containing the lower and upper limit for x values displayed
}
  \item{clim}{
%%     ~~Describe \code{input_xsize} here~~
Vector containing the lower and upper limit for size values displayed
}
  \item{slim}{
%%     ~~Describe \code{input_ysize} here~~
Vector containing the lower and upper limit for color values displayed
}
  \item{px}{
%%     ~~Describe \code{px} here~~
Pixelsize can used to scale the image to desirable size of points
}
  \item{grey}{
%%     ~~Describe \code{grey} here~~
Displays the graph in a greyscale if TRUE
}
  \item{split_ch}{
%%     ~~Describe \code{split_ch} here~~
If TRUE and a channel collum was present the output will be divided over multiple graphs
IF FALSE a single graph will be made with containing all channels
}
  \item{fit}{
%%     ~~Describe \code{fit} here~~
If fit is TRUE values will be scaled to fit within the plot
}
  \item{color_scale}{
Add your own color scale to the plot. The values in color are automatically scaled between the first and last color in the scale. Using adjustcolor() transparency can be added to the color scale.
}
  \item{alpha}{
A number between 0 and 1 indicating the transparancy of the plotted points  
}
  \item{overlay}{
smolr_image or file location of a tiff file to which the plot will be overlayed    
}
  \item{contrast}{
Value indicating what percentage of saturated pixels will be in the overlay (i.e. 1.2 will give 20 percent saturated pixes)  
}
  \item{color_scale}{
vector containing a custom color scale  
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
Optical Imaging Center Rotterdam
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
SMOLR_PLOT(smolrdata,rev.color=TRUE,size="off",split_ch=TRUE)

SMOLR_PLOT(smolrdata,color_scale = adjustcolor(c("red","green","magenta"),0.3))
}

