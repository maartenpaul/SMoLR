\name{SMLMR_FEATURES}
\alias{SMLMR_FEATURES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get feature analysis from smlmr_kde data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns a feature analysis of a smlmr kernel density estimation.
}
\usage{
smlmr_features(x, filter = NULL, filter_value = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A single molecule kernel density estimation 
}
  \item{filter}{
%%     ~~Describe \code{filter} here~~
name of an feature to filter in the parameter setting
}
  \item{filter_value}{
%%     ~~Describe \code{filter_value} here~~
Value to filter, counts features that have a value higher than filter_value
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Feature analysis as present in the EBImage package
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{features}{A table of features measured per channel}
\item{parameters }{number of features per channel and number of filtered features}
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

test_kde <- SMLMR_KDE(c(200,150,400,210),c(200,300,400.5,239),ch=c(2,3,4,3),threshold=0.01)

test_features <- SMLMR_FEATURES(test_kde)
test_features <- SMLMR_FEATURES(test_kde, filter="x.0.s.area", filter_value=1000)

}