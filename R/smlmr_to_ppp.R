smlmr_to_ppp <- function(x,y,xlim,ylim,marks=NULL,shape="rect"){
  
  if(shape=="rect"){
    window <- owin(xlim,ylim)  
  }
  if(shape=="circle"){
    window <- disc(radius = ((xlim[2]-xlim[1])+(ylim[2]-ylim[1]))/2,centre = c(mean(xlim),mean(ylim)))
  }
  
  if(is.null(marks)){
    out <- ppp(x = x,y = y,window = window)
  } else {
    out <- ppp(x = x,y = y,window = window,marks = as.factor(marks))
  }
  
  return(out)
}



SMLMR_TO_PPP <- function(x,y,xlim,ylim,marks,shape){
  UseMethod("smlmr_to_ppp")
}

SMLMR_TO_PPP.default <- function(x,y,xlim,ylim,marks=NULL,shape="rect"){
  smlmr_to_ppp(x,y,xlim,ylim,marks,shape)
}

SMLMR_TO_PPP.data.frame <- function(x,xlim=NULL,ylim=NULL,marks=NULL,shape="rect"){
  
  ind_x <- grep("^x$",names(x),ignore.case=T)
  ind_y <- grep("^y$",names(x),ignore.case=T)
  ind_ch <- grep("^ch",names(x),ignore.case=T)
  
  
  
  if(length(c(ind_x,ind_y,ind_ch))!=3){stop("Not all parameters (x,y,channel,precision) are present once in the header")}
  
  dx <- x[,ind_x]
  y <- x[,ind_y]
  ch <- x[,ind_ch]
  
  if(is.null(xlim)||is.null(ylim)){
    xlim <- c(min(dx)-10,max(dx)+10)
    ylim <- c(min(y)-10,max(y)+10)
  }  
  out <- smlmr_to_ppp(x=dx,y=y,marks=ch,xlim=xlim,ylim=ylim,shape=shape)
  return(out)
}

SMLMR_TO_PPP.list <- function(x,xlim,ylim,shape="rect"){
  out <- list()
  for(i in 1:length(x)){
    out [[i]] <- smlmr_to_ppp(x[[i]],xlim=as.numeric(xlim[i,]),ylim=as.numeric(ylim[i,]),shape=shape)
    
  }
  return(out)
}