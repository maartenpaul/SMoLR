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

IJROI_subset <- function(x,file,pxsize,split){
  UseMethod("IJROI_subset")
}

IJROI_subset.default <- function(x,file,pxsize=5,split=F){
  
  if(file_ext(file)=="zip"){
    D <- floor(x$Y/pxsize)
    C <- floor(x$X/pxsize)
    data <- RImageJROI::read.ijzip(file)
    data <- llply(data,function(x){
      if (x$strType=="traced"){
        x$strType="polygon"
      }
      return(x)
    })
    class(data) <- "ijzip"
    
    maskdata<-ij2spatstat(data)  
    
    out <- llply(maskdata,function(x){
      inside.owin(x = C,y = D,w=x)
      
      
    })
    for (i in 1:length(out)){
      x$inroi[out[[i]]] <- data[[i]]$name
    }
    
  } else if (file_ext(file)=="roi"){
    D <- floor(x$Y/pxsize)
    C <- floor(x$X/pxsize)
    data <- read.ijroi(file)
    maskdata<-ij2spatstat(data)  
    isin<-inside.owin(x = C,y = D,w=maskdata)
    x$inroi <- isin
  }
  if (split){
  x <- na.omit(x)
  x <- dlply(x,.variables = "inroi")
  }
  return(x)
}

IJROI_subset.list <- function(x,file,pxsize=5,split=F){
 if(length(x)==length(file)){
   y <- list()
   for(i in 1:length(x)){
     y[[names(x)[i]]] <- IJROI_subset(x[[i]],file[i],pxsize,split)
   }
   if (split){
     y <- unlist(y,recursive = F)
   }
 } else{
   stop("localizations list and list of file of different length")
 }
 return(y)
}
#example
#locs <- IJROI_subset(localizations[[1]],file,5)

