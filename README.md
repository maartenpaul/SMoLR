## SMoLR
Single Molecule Localization in R  

## Installation

install.packages("devtools")  
library(devtools)  
source("https://bioconductor.org/biocLite.R")  
biocLite("EBImage")  

install_github("ErasmusOIC/SMoLR", build_vignettes = TRUE)


library(SMoLR)  


## Documentation
Documentation of the different functions in the package can be found in R. Additionally a vignette is available explaining the main functions of SMoLR.
The vignette can be found here:
https://htmlpreview.github.io/?https://github.com/ErasmusOIC/SMoLR_data/blob/master/SMolR.html

or within R, after installing SMoLR, run:
library(SMoLR)
vignette("SMolR")

## Example data
Example data to test the package can be found at the repository: https://github.com/ErasmusOIC/SMoLR_data


download.file("https://github.com/ErasmusOIC/SMoLR_data/raw/master/thunderstorm.zip","thunderstorm.zip")
unzip("thunderstorm.zip")
SMOLR_IMPORT(file.path(getwd(),"thunderstorm"),profile="thunderstorm")
SMOLR_LOAD(file.path(getwd(),"thunderstorm"),statistics=F)
head(localizations[[1]])

## Bug reports and feature requests
Bug reports, feature requests, or any other issues with the package can be reported at github.
