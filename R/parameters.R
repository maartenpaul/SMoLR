getParameters <- function(x,y,ch=NULL,prec=NULL,ch_range=NULL){
  
  if(is.null(ch)){ch <- rep(1, length(x))}
  if(is.null(ch_range)){ch_range <- unique(ch)}
  if(is.null(prec)){prec <- rep(NA, length(x))}
 
  nloc <- rep(0, length(ch_range))
  mean_prec <-rep(0,length(ch_range))
  min_prec <-rep(0,length(ch_range))
  max_prec <-rep(0,length(ch_range))
  channel <- rep(0,length(ch_range))
  max_x <- rep(0,length(ch_range))
  min_x <- rep(0,length(ch_range))
  max_y <- rep(0,length(ch_range))
  min_y <- rep(0,length(ch_range))
  mean_x <- rep(0,length(ch_range))
  mean_y <- rep(0,length(ch_range))
  
  
  for(i in 1:length(ch_range)){
    
    if(length(which(ch==ch_range[i]))>0){
    channel[i] <- ch_range[i]
    nloc[i] <- length(x[ch==ch_range[i]])
    mean_prec[i] <- mean(prec[ch==ch_range[i]])
    min_prec[i] <- min(prec[ch==ch_range[i]])
    max_prec[i] <- max(prec[ch==ch_range[i]])
    max_x[i] <- max(x[ch==ch_range[i]])
    min_x[i] <- min(x[ch==ch_range[i]])
    max_y[i] <- max(y[ch==ch_range[i]])
    min_y[i] <- min(y[ch==ch_range[i]])
    mean_x[i] <- mean(x[ch==ch_range[i]])
    mean_y[i] <- mean(y[ch==ch_range[i]])
    }
    if(length(which(ch==ch_range[i]))==0){
      channel[i] <- ch_range[i]
      nloc[i] <- NA
      mean_prec[i] <- NA
      min_prec[i] <- NA
      max_prec[i] <- NA
      max_x[i] <- NA
      min_x[i] <- NA
      max_y[i] <- NA
      min_y[i] <- NA
      mean_x[i] <- NA
      mean_y[i] <- NA 
    }
    
  }
    
  parameters <- data.frame(cbind(channel,nloc,mean_prec,min_prec,max_prec,min_x,max_x,min_y,max_y,mean_x,mean_y))
  return(parameters)
}

SMLMR_PARAMETER <- function(x,y,ch,prec,ch_range){
  UseMethod("SMLMR_PARAMETER")
}

SMLMR_PARAMETER.default <- function(x,y,ch=NULL,prec=NULL,ch_range=NULL){
  parameters <- getParameters(x,y,ch,prec,ch_range)
  return(parameters)
}

SMLMR_PARAMETER.data.frame <- function(x,y=NULL,ch=NULL,prec=NULL,ch_range=NULL){
  
  ind_x <- grep("^x$",names(x),ignore.case=T)
  ind_y <- grep("^y$",names(x),ignore.case=T)
  ind_ch <- grep("^ch",names(x),ignore.case=T)
  ind_prec <- grep("^prec",names(x),ignore.case=T)
  
  if(length(c(ind_x,ind_y,ind_prec,ind_ch))!=4){stop("Not all parameters (x,y,channel,precision) are present once in the header")}
    
  dx <- x[,ind_x]
  y <- x[,ind_y]
  prec <- x[,ind_prec]
  ch <- x[,ind_ch]

  parameters <- getParameters(dx,y,ch,prec,ch_range)
  
  return(parameters)
}

SMLMR_PARAMETER.list <- function(x,y=NULL,ch=NULL,prec=NULL,ch_range=NULL){
  
  parameters <- list()
  
  for(i in 1:length(x)){
  parameters[[i]] <- SMLMR_PARAMETER(x[[i]],y,ch,prec,ch_range)
  }
  
  return(parameters)
}


