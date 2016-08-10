\name{plot.smolr_image}
\alias{plot.smolr_image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PLOT of SMOLR image
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.smolr_image(x, y, saturate = 0, brightness = 0, contrast = 1, rgb = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{saturate}{
%%     ~~Describe \code{saturate} here~~
}
  \item{brightness}{
%%     ~~Describe \code{brightness} here~~
}
  \item{contrast}{
%%     ~~Describe \code{contrast} here~~
}
  \item{rgb}{
%%     ~~Describe \code{rgb} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, saturate = 0, brightness = 0, contrast = 1, rgb = F, 
    ...) 
{
    if (class(x[[1]]) == "character") {
        print(x[[1]])
    }
    else {
        if (rgb) {
            x$img <- aperm(x$img, c(2, 1, 3))
            x$img <- EBImage::flop(x$img)
            if (length(contrast == 1)) {
                contrast <- rep(contrast, length(x[[2]]$channel))
            }
            if (length(saturate == 1)) {
                saturate <- rep(saturate, length(x[[2]]$channel))
            }
            if (length(brightness == 1)) {
                brightness <- rep(brightness, length(x[[2]]$channel))
            }
            if (length(x[[2]]$channel) > 3) {
                stop("Maximum of three channels for RGB images")
            }
            for (i in 1:length(x[[2]]$channel)) {
                img <- x$img[, , i]
                max <- sort(img)[length(img) * (1 - saturate[i])]
                img <- img/max
                img <- img + brightness[i]
                img <- img * contrast[i]
                img[img > 1] <- 1
                img[img < 0] <- 0
                x$img[, , i] <- img
            }
            if (length(x[[2]]$channel) == 1) {
                col <- rgb(x[[1]][, , 1], matrix(data = 0, ncol = ncol(x[[1]][, 
                  , 1]), nrow = nrow(x[[1]][, , 1])), matrix(data = 0, 
                  ncol = ncol(x[[1]][, , 1]), nrow = nrow(x[[1]][, 
                    , 1])))
                dim(col) <- dim(x[[1]][, , 1])
            }
            if (length(x[[2]]$channel) == 2) {
                col <- rgb(x[[1]][, , 1], x[[1]][, , 2], matrix(data = 0, 
                  ncol = ncol(x[[1]][, , 1]), nrow = nrow(x[[1]][, 
                    , 1])))
                dim(col) <- dim(x[[1]][, , 1])
            }
            if (length(x[[2]]$channel) == 3) {
                col <- rgb(x[[1]][, , 1], x[[1]][, , 2], x[[1]][, 
                  , 3])
                dim(col) <- dim(x[[1]][, , 1])
            }
            oripar <- par(mar = c(2.5, 2.5, 2.5, 2.5), no.readonly = TRUE)
            plot(1:2, type = "n", xlim = c(x$inputs$xlim[1], 
                x$inputs$xlim[2]), ylim = c(x$inputs$ylim[1], 
                x$inputs$ylim[2]), asp = 1, xlab = "X", ylab = "Y", 
                bty = "n", axes = FALSE)
            lim <- par()
            rasterImage(col, x$inputs$xlim[1], x$inputs$ylim[1], 
                x$inputs$xlim[2], x$inputs$ylim[2])
            par(oripar)
        }
        else {
            oripar <- par(pty = "s", mfrow = c(1, length(x$inputs$ch_range)), 
                mar = c(2.5, 2.5, 2.5, 2.5), no.readonly = TRUE)
            for (i in 1:length(x$inputs$ch_range)) {
                img <- x$img[, , i]
                max <- sort(img)[length(img) * (1 - saturate)]
                img <- img/max
                img <- img + brightness
                img <- img * contrast
                img[img > 1] <- 1
                img[img < 0] <- 0
                image(x = seq(1, dim(x$img[, , i])[1] * x$inputs$px, 
                  length.out = dim(x$img[, , i])[1]), y = seq(1, 
                  dim(x$img[, , i])[2] * x$inputs$px, length.out = dim(x$img[, 
                    , i])[2]), z = img, main = paste("channel", 
                  x$inputs$ch_range[i], sep = " "), col = grey.colors(2^16, 
                  start = 0, end = 1), xlab = "", ylab = "", 
                  xlim = c(1, dim(x$img[, , i])[1] * x$inputs$px), 
                  ylim = c(1, dim(x$img[, , i])[2] * x$inputs$px), 
                  asp = 1, useRaster = T)
            }
            par(oripar)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
