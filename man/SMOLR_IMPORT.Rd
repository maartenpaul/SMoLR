\name{SMOLR_IMPORT}
\alias{SMOLR_IMPORT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Import Single Molecule Localization Data into SMOLR
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

}
\usage{
SMOLR_IMPORT(folder, basename, sep_chfiles, channel, length_statistics, condition, remove_empty_ROI, extension, prename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folder}{
%%     ~~Describe \code{folder} here~~
The folder containing the localization data files
}
  \item{basename}{
%%     ~~Describe \code{basename} here~~
}
  \item{sep_chfiles}{
%%     ~~Describe \code{sep_chfiles} here~~
If multiple channels are present are they present in one text file or separate files
}
  \item{channel}{
%%     ~~Describe \code{channel} here~~
Which channels are presents, default=1
}
  \item{length_statistics}{
%%     ~~Describe \code{length_statistics} here~~
If ROI statistics are present, under the localization data in the text files, the number of lines that should be converted into a statistics file is shown here
}
  \item{condition}{
%%     ~~Describe \code{condition} here~~
The data can be labelled with a condition, which allows comparing different experimental conditions
}
  \item{profile}{
%%     ~~Describe \code{condition} here~~
Profiles with correct settings for different data types. Currently available: "default","loc","roiloc","elyra",'thunderstorm'
}
  \item{remove_empty_ROI}{
%%     ~~Describe \code{remove_empty_ROI} here~~
If ROIs are present that do not contain any localization they will be removed
}
  \item{extension}{
%%     ~~Describe \code{extension} here~~
}
  \item{prename}{
%%     ~~Describe \code{prename} here~~
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
SMOLR_LOAD
}
\examples{

SMOLR_IMPORT(file.path(getwd(),"loc"),profile="roiloc")
SMOLR_LOAD(file.path(getwd(),"loc"),statistics=T)

SMOLR_IMPORT(file.path(getwd(),"thunderstorm"),profile="thunderstorm")

## The function is currently defined as
function (folder, basename, sep_chfiles, channel, length_statistics, 
    condition, remove_empty_ROI, extension, prename) 
{
    UseMethod("smlmr_import")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
