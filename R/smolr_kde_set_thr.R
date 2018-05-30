smolr_kde_setThreshold <- function(kde = NULL, threshold=0.05){
  
  if(class(kde)!="smolr_kde"){stop("kde class is not 'smolr_kde'")}
  
  kde_raw <- kde$kde
  kde_bin <- kde$kde_binary
  
  
  dimensions <- dim(kde_raw)
  
  kern <- EBImage::makeBrush(3, shape="disc")
  
  for(i in 1:dimensions[3]){
    
    kde_binary <- kde_raw[,,i]
    kde_binary[kde_binary < threshold] <- 0
    kde_binary[kde_binary >= threshold] <- 1
    kde_binary <- EBImage::erode(kde_binary, kern)    
    kde_binary <- EBImage::dilate(kde_binary, kern)
    
    kde_bin[,,i] <- kde_binary
    
  }
  
  kde$inputs$threshold <- threshold
  kde$kde_binary <- kde_bin
  
  return(kde)
  
  
}

SMOLR_KDE_SET_THR <- function(kde,threshold){
  UseMethod("SMOLR_KDE_SET_THR")
}

SMOLR_KDE_SET_THR.default <- function(kde = NULL,threshold=0.05){
  kde <- smolr_kde_setThreshold(kde,threshold)
  
  return(kde)
}

SMOLR_KDE_SET_THR.list <- function(kde=NULL,threshold=0.05){
  kdes <- list()
  
  for(i in 1:length(kde)){
    kdes[[i]] <- smolr_kde_setThreshold(kde[[i]],threshold)  
  }
  
  return(kdes)
}


