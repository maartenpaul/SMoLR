\name{IJROI_subset}
\alias{IJROI_subset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ImageJ ROI subset localizations file
}
\description{
This function can be used to subset a localization file based on ROIs (Region of Interest) defined by ImageJ. This can be one or multiple ROIs.
}
\usage{
IJROI_subset(x, file, pxsize = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A data.frame or list of data.frames with single molecule localizations, can be generated using \code{\link{SMOLR_IMPORT}}
}
  \item{file}{
The file location on the ROI file to be processd, either a single ROI (.roi) or multiple ROIs (.zip) can be a vector of file locations equal to the number of localizations files in a list.
}
  \item{pxsize}{
The pixel size used to create the ROI files in ImageJ defined in nanometer, default = 5 nm. 
}
  \item{split}{
  If TRUE, the localizations data.frame will be splitted in a list of data.frames for every ROI, default = FALSE.
}
}
\details{
WARNING: For now overlapping ROIs are not supported, localizations will be present only in one IJ-ROI.
}
\value{
Returns the same (list of) data.frame, indicating for every localization to which ROI they belong. If split=TRUE, for every ROI a dat.frame is returned in a list.
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
\code{\link{read.ijroi}}
\code{\link{read.ijzip}}
}
\examples{
###Example can be downloaded from the SMOLR_DATA GitHub
#data <- IJROI_subset(smolrdata,file = "IJROI_data/RoiSet.zip")
#data <- IJROI_subset(smolrdata,file = "IJROI_data/RoiSet.zip",split=T)
#plot(SMOLR(data[[1]]),rgb=T)
#plot(SMOLR(data[[2]]),rgb=T)
#plot(SMOLR(data[[3]]),rgb=T)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

