\name{SMOLR_IMPORT}
\alias{SMOLR_IMPORT}
\alias{SMOLR_LOAD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Import Single Molecule Localization Data into R
}
\description{
From different dataformats the SMOLR_IMPORT function can import data from a chosen directory into a list of data.frames and saves it to a .Rdata file in the same directory.
The SMOLR_LOAD function can subsequently be used to load the data from the directory into the R evironment.

}
\usage{
SMOLR_IMPORT(folder, basename, sep_chfiles, channel, length_statistics, condition, remove_empty_ROI, extension, prename)
SMOLR_LOAD(folder, statistics)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folder}{
The folder/directory containing the localization data (text) files.
}
  \item{basename}{
The part of the file name which every text file to be imported has. This is followed by an ID/number.
}
  \item{sep_chfiles}{
%%     ~~Describe \code{sep_chfiles} here~~
If the data contains multiple image channels are they present in one text file or separate files, default=FALSE. If TRUE the channel names should be idicated with channel and should be present in the to-be imported filenames after the basename.
}
  \item{channel}{
%%     ~~Describe \code{channel} here~~
Which channels are presents, default=1. If multiple channels are present give their names in a \code{vector}.
}
  \item{length_statistics}{
%%     ~~Describe \code{length_statistics} here~~
If ROI statistics are present, under the localization data in the text files, the number of lines that should be converted into a statistics file is shown here. Only the case when the ImageJ SMLM_viewer is used to organize localization data. Those channel names should be present in the to-be imported filenames after the basename.
}
  \item{condition}{
%%     ~~Describe \code{condition} here~~
The data can be labelled with a condition, which allows comparing different experimental conditions, when different datasets are imported.
}
  \item{profile}{
%%     ~~Describe \code{condition} here~~
Profiles with correct settings for different data types. Currently available: default="default".Using SMLM_viewer macro in ImageJ/Fiji: "loc" and "roiloc". For Zeiss Elyra: "elyra". For the ImageJ/Fiji plugin Thunderstorm: 'thunderstorm'.
}
  \item{remove_empty_ROI}{
%%     ~~Describe \code{remove_empty_ROI} here~~
If ROIs are present that do not contain any localization they will be removed
}
  \item{extension}{
File extension of the to-be imported localization files. default is txt .
}
  \item{prename}{
Is there any name in the file names of the localization files that can be used to filter the to-be imported files from the folders.
}
 \item{statistics}{
  Using SMOLR_LOAD should the attached statistics file be loaded, default is FALSE.
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
ImageJ/Fiji plugin Thunderstorm: https://github.com/zitmen/thunderstorm .
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
#see for example files SMoLR_data github
#https://github.com/ErasmusOIC/SMoLR_data/raw/master/loc.zip
SMOLR_IMPORT(file.path(getwd(),"loc"),profile="roiloc")
SMOLR_LOAD(file.path(getwd(),"loc"),statistics=T)
#Example Thuderstorm file:
#https://github.com/ErasmusOIC/SMoLR_data/raw/master/thunderstorm.zip
SMOLR_IMPORT(file.path(getwd(),"thunderstorm"),profile="thunderstorm")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

