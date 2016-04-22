\name{SMLMR_KDE}
\alias{SMLMR_KDE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get kernel density estimation from a single molecule dataset
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Fits a kernel density estimation to single molecule localization data based on bkde2de from the kernsmooth package
}
\usage{
SMLMR_KDE(x, y, ch, prec, bandwidth, input_xsize, input_ysize, px, threshold, fit, file, output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
X coordinates in nm
}
  \item{y}{
%%     ~~Describe \code{y} here~~
Y coordinates in nm
}
  \item{ch}{
%%     ~~Describe \code{ch} here~~
channel number (default=1)
}
  \item{prec}{
%%     ~~Describe \code{prec} here~~
precision in nm (default=20)
}
  \item{bandwidth}{
%%     ~~Describe \code{bandwidth} here~~
Bandwidth of the kernel used to fit, typically a bandwidth of around the precision is used. The bandwidth is a vector containing an x and a y size.
}
  \item{xlim}{
%%     ~~Describe \code{input_xsize} here~~
Vector with the upper and lower limit of x axis (x coordinates outside limits will be discarded if fit=TRUE)
}
  \item{ylim}{
%%     ~~Describe \code{input_ysize} here~~
Vector with the upper and lower limit of y axis (y coordinates outside limits will be discarded if fit=TRUE)
}
  \item{px}{
%%     ~~Describe \code{px} here~~
pixelsize in nm
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
threshold for the kde in loxalizations per squared nm, used to create binary image of the single molecule data
}
  \item{fit}{
%%     ~~Describe \code{fit} here~~
If fit=TRUE all coordinates that lie outside of indicated xlim and ylim values are discarded
}
\item{file}{
path and name of file to save tif to (i.e. file="D:/test.tif", will return the files test_kde.tif and test_kde_bin.tif)
}
\item{output}{
Set output to "tiff" to output tiff files of the KDE
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{kde }{Array containing the kernel density estimation}
\item{kde_binary }{Array containing the binary thresholded image }
\item{parameters}{Parameters of the input}
\item{int}{list of intensities from the kde per localization, unique feature id per localization}
\item{inputs}{list of values that were the input in the function, for back reference}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
kernel density estimation uses the bkde2D algorithm from the kernsmooth package
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
test_kde <- SMLMR_KDE(400,400,threshold=0.005)
test_kde <- SMLMR_KDE(c(200,150,400,345),c(200,300,400.5,239),ch=c(2,3,4,2),threshold=0.01)
test_kde <- SMLMR_KDE(smlmrdata)


#test_kde
#test_kde$kde
#test_kde$kde_binary
#test_kde$parameters
#test_kde$int
plot(test_kde)
SMLMR_PLOT(smlmrdata,color=SMLMR_KDE(smlmrdata)[[4]][,1],rev.color=TRUE,size="off",split_ch=TRUE)
}
