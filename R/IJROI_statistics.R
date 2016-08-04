library(RImageJROI)
IJROI_statistics <- function(file,pxsize=5){
  if(file_ext(file)=="zip"){
    data <- RImageJROI::read.ijzip(file)
    out <- ldply(data,function(x) c(x$xrange*pxsize,x$yrange*pxsize))
    names(out) <- c("ROI_id","ROI_xmin","ROI_xmax","ROI_ymin","ROI_ymax")
  } else if (file_ext(file)=="roi"){
    data <- read.ijroi(file)
    tmp <- c(data$xrange*pxsize,data$yrange*pxsize)
    out <- data.frame("ROI_id"=1,"ROI_xmin"=0,"ROI_xmax"=0,"ROI_ymin"=0,"ROI_ymax"=0)
    out[1,2:5] <- tmp
  }
  invisible(out)
}