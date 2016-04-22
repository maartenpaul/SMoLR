\name{SMLMR}
\alias{SMLMR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Single Molecule Localization Gaussian Image Reconstruction
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Visualizes single molecule data (x,y,precision,channel) by plotting a gaussian function with a sigma proportional to the precision of the localizations, and a fixed integrated density.
}
\usage{
SMLMR(x, y, prec, ch, px, xlim, ylim, file, output, fit, fast)
\method{SMLMR}{data.frame}(x, px, xlim, ylim, file, output, fit, fast)
\method{SMLMR}{list}(x, px, xlim, ylim, file, output, fit, fast)
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
  \item{prec}{
%%     ~~Describe \code{prec} here~~
Precisions in nm, if not specified 20nm is used
}
  \item{ch}{
%%     ~~Describe \code{ch} here~~
Channel number, if not specified channel is 1
}
  \item{px}{
%%     ~~Describe \code{px} here~~
Pixelsize in nm of the final image, default=5 nm
}
  \item{xlim}{
%%     ~~Describe \code{input_xsize} here~~
Vector with the upper and lower limit of x axis (x coordinates outside limits will be discarded if fit=TRUE) 
}
  \item{ylim}{
%%     ~~Describe \code{input_ysize} here~~
Vector with the upper and lower limit of y axis  (y coordinates outside limits will be discarded if fit=TRUE) 
}
  \item{output}{
%%     ~~Describe \code{output} here~~
Type of output: "tiff" will output a tif image as indicated in file. "r" will output an smlmr_image object. Default is "r". 
}
  \item{file}{
%%     ~~Describe \code{file} here~~
Filename to output tiff image to when output is "tiff" if no filename is given a dialog box opens and asks for a filename
}
  \item{fit}{
%%     ~~Describe \code{fit} here~~
If fit=TRUE all coordinates that lie outside of indicated xlim and ylim values are discarded
}
  \item{fast}{
%%     ~~Describe \code{fast} here~~
If fast is TRUE a predefined list of gaussian functions will be loaded in the memory instead of generated for each localization. This Will speed up calculations for large images, loses some accuracy by placing gaussians in the middle of the pixel.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
returns a r object of the class smlmr_image which is a list containing 3 items
%%  If it is a LIST, use
\item{img}{an array pixel values between zero and one}
\item{parameters}{list of parameters of the given data per channel. Nloc number of localizations, max min and mean of x and y coordinates}
\item{inputs}{list of values that were the input in the function, for back reference}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
test <- SMLMR(c(200,150,400,345),c(200,300,400.5,239),ch=c(2,3,4,2), prec=c(15,10,16.3,12))

#test #displays a summery 
#test$img #displays the image array
#test$parameters #displays the parameters of the image
#plot(test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

