\name{SMOLR_IMPORT}
\alias{SMOLR_IMPORT}
\alias{SMOLR_LOAD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Import Single Molecule Localization Data into R
}
\description{
From different dataformats the SMOLR_IMPORT function can import data from a chosen folder into a list of data.frames and converts it to a .Rdata file in the same folder.
The SMOLR_LOAD function can subsequently be used to load the data from the folder into the R evironment.

}
\usage{
SMOLR_IMPORT(folder, basename, sep_chfiles, channel, length_statistics, condition, remove_empty_ROI, extension, prename)
SMOLR_LOAD(folder, statistics)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folder}{
The folder containing the localization data files
}
  \item{basename}{
The part of the file name which every text file to be imported has. This is followed by a number.
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
If ROI statistics are present, under the localization data in the text files, the number of lines that should be converted into a statistics file is shown here. Only the case when the ImageJ SMLM_viewer is used to organize localization data.
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
File extenstion of to be imported localization files
}
  \item{prename}{
Is there any name in the file names of the localization files that can be used to subset the imported files from the folders 
}
 \item{statistics}{
  Using SMOLR_LOAD should the attached statistics file be loaded, default to FALSE
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
SMOLR_IMPORT(file.path(getwd(),"loc"),profile="roiloc")
SMOLR_LOAD(file.path(getwd(),"loc"),statistics=T)

SMOLR_IMPORT(file.path(getwd(),"thunderstorm"),profile="thunderstorm")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

