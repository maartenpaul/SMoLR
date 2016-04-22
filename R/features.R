
smlmr_features <- function(x, filter=NULL, filter_value=NULL){
  
  
  est <- array(0,c(dim(x[[1]])[1],dim(x[[1]])[2],dim(x[[1]])[3]))
  features <- list()
  parameters <- data.frame(matrix(ncol=0,nrow=length(x[[3]]$channel)))
  
  for(i in 1:length(x[[3]]$channel)){
  
  est[,,i] <- bwlabel(x[[2]][,,i])
  channelname <- paste("channel",x[[3]]$channel[i], sep="_")
  features[channelname] <- list(EBImage::computeFeatures(x=est[,,i],ref=x[[1]][,,i],properties=F,expandRef=NULL))
  
  parameters$channel[i] <- x[[3]]$channel[i]
  if(is.null(features[[i]])){parameters$No_features[i] <- 0}
  if(!is.null(features[[i]])){parameters$No_features[i] <- nrow(features[[i]])}
  if(is.null(filter) || is.null(filter_value) || is.na(tryCatch(data.frame(features[[i]])[filter], error = function(e) NA)) ) {parameters$filtered_features[i] <- 0
                                                                                  parameters$filtered_features_id[i] <- 0}
  
  if(!is.null(filter) && !is.null(filter_value) && !is.na(tryCatch(data.frame(features[[i]])[filter], error = function(e) NA))) {parameters$filtered_features[i] <- length(data.frame(features[[i]])[filter][data.frame(features[[i]])[filter]>filter_value])
                                                  parameters$filtered_features_id[i] <- paste0(which(data.frame(features[[i]])[filter]>filter_value),collapse=" ")}
  
  }
  
  features["parameters"] <- list(parameters)
  
  return(features)
  
  
}

SMLMR_FEATURES <- function(x, filter, filter_value){
  UseMethod("SMLMR_FEATURES")
}


SMLMR_FEATURES.default <- function(x, filter=NULL, filter_value=NULL){
  cat("x must be of the class smlmr_kde")
}

SMLMR_FEATURES.smlmr_kde <- function(x, filter=NULL, filter_value=NULL){
  features <- smlmr_features(x, filter,filter_value)
  return(features)
}

SMLMR_FEATURES.list <- function(x, filter=NULL, filter_value=NULL){
  
  features <- list()
  
  for(i in 1:length(x)){
    features[[i]] <- SMLMR_FEATURES(x[[i]], filter,filter_value)
  }
  
  return(features)
}




