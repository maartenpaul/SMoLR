\name{SMOLR-package}
\alias{singlemoleculer-package}
\alias{singlemoleculer}
\docType{package}
\title{
SMoLR package overview

}
\description{
Single Molecule Localization in R (SMoLR) is a package for analysis in single-molecule localization microscopy data in R. The pacakage allows to extract spatial organization information from (multiple) single-molecule super-resolution images or ROIs (Region of Interests). Using lists large sets of localization data organized in a \code{list} of \code{data.frame}'s can be applied to the functions of the package.
}
\details{
\tabular{ll}{
Package: \tab SMoLR\cr
Type: \tab Package\cr
Version: \tab 0.9.2\cr
Date: \tab 2017-08-07\cr
License: \tab LGPLv3\cr
}
}
\section{Package content}{
Data import:
  \itemize{
    \item \code{SMOLR_IMPORT}
    \item \code{SMOLR_LOAD}
    \item \code{IJROI_subset}
  }
  
Visualization:
  \itemize{
    \item \code{SMOLR}
    \item \code{SMOLR_KDE}
    \item \code{SMOLR_PLOT}
  }

Clustering:
  \itemize{
    \item \code{SMOLR_KDE}
    \item \code{SMOLR_DBSCAN}
  }
  
Statistic extration:
  \itemize{
    \item \code{SMOLR_FEATURES}
    \item \code{SMOLR_POINT_FEATURES}
    \item \code{SMOLR_TO_PPP}
  }


}

\author{
Optical Imaging Center Erasmus MC, Rotterdam\cr

Johan Slotman (j.slotman@erasmusmc.nl) and Maarten Paul (m.w.paul@erasmusmc.nl)
}
\references{

}

\keyword{ package }
\seealso{

}
\examples{
  vignette("vignettename")
}