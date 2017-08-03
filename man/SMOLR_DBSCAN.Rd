\name{SMOLR_DBSCAN}
\alias{SMOLR_DBSCAN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Analysis of SMOLR localization data by DBSCAN clustering, using the DBSCAN algorithm from the DBSCAN R package.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SMOLR_DBSCAN(x, y, ch, prec, eps, MinPts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
X coordinates in nm
}
  \item{y}{
Y coordinates in nm
}
  \item{ch}{
channel number (default=1)
}
  \item{prec}{
precision in nm (default=20)
}
  \item{eps}{
size of linking radius. (default = 50)
}
  \item{MinPts}{
number of minimum points in the eps region (default =50)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a r object of the class smolr_dbscan which is a list containing 4 items.
\item{dbscan}{the localization data.frame containing an addition "Cluster" column, indicating the cluster ID as determined by the dbscan algorithm. Cluster=0 is background}
\item{parameters}{list of parameters of the given data per channel. Nloc number of localizations, max min and mean of x and y coordinates.}
\item{clust_parameters}{list of parameters of the given data per DBSCAN cluster.}
\item{inputs}{list of values that were the input in the function, for back reference}

The smolrdata object can be used for plotting.


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
\code{\link{dbscan}}
}
\examples{
x <- SMOLR_DBSCAN(smolrdata)
plot(x)
plot(x,hide_noise = T)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}% use one of  RShowDoc("KEYWORDS")
