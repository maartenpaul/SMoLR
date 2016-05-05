smolr_kde <- function(x,y,ch=NULL,prec=NULL, bandwidth= c(20,20), xlim=NULL, ylim=NULL, px=5, threshold=0.05, file=NULL, output=c("r","tiff"), fit = TRUE){
  
  
  if((is.null(xlim) || length(xlim)==2)==FALSE){stop("xlim should be a vector with two values")}
  if((is.null(ylim) || length(ylim)==2)==FALSE){stop("ylim should be a vector with two values")}  
  
    
  if(is.null(xlim)==FALSE) {
    input_xsize <- ((xlim[2]-xlim[1])/px)+2
    input_xsize <- ceiling(input_xsize/px)*px #make multiple of px
    x <- x-xlim[1]
    
  }  
  if(is.null(ylim)==FALSE) {
    input_ysize <- ((ylim[2]-ylim[1])/px)+2
    input_ysize <- ceiling(input_ysize/px)*px
    y <- y-ylim[1]
  } 
  if(is.null(xlim)){
    input_xsize <- ((max(x)-min(x))/px)+2
    input_xsize <- ceiling(input_xsize/px)*px
    x <- x - min(x)
  }
  if(is.null(ylim)){
    input_ysize <- ((max(y)-min(y))/px)+2
    input_ysize <- ceiling(input_ysize/px)*px
    y <- y - min(y)
  }
  
  
  output <-match.arg(output)
  
  ch_range <- unique(ch)
  
  if(fit==TRUE){
    
    selection <- x>0 & x<input_xsize*px & y>0 & y<input_ysize*px
    x <- x[selection]
    y <- y[selection]
    prec <- prec[selection]
    ch <- ch[selection]
    
  }
  
  
      
  if(max(x)>input_xsize*px||max(y)>input_ysize*px || min(x)<0 ||min(y)<0 ){stop("X or y out of bound.")}
    
  if(is.null(ch)){ch <- rep(1,length(x))}
  
  range <- list(c(0,input_xsize*px),c(0,input_ysize*px))
  
  kde <- array(data=0, dim=(c(input_xsize,input_ysize,length(ch_range))))
  kdebin <- array(data=0, dim=(c(input_xsize,input_ysize,length(ch_range))))
  
  kern <- makeBrush(3, shape="disc")
  
    
  for(i in 1:length(ch_range)){
    
    if(length(which(ch==ch_range[i]))>0){
    inp <- cbind(x[ch==ch_range[i]],y[ch==ch_range[i]])
    
    kde_temp <- KernSmooth::bkde2D(x=inp, bandwidth= bandwidth, gridsize = c(input_xsize,input_ysize), range.x= range )
    kde_temp$fhat <- kde_temp$fhat*px*px*nrow(inp)
    
    kde_binary <- kde_temp$fhat
    kde_binary[kde_binary < threshold] <- 0
    kde_binary[kde_binary >= threshold] <- 1
    kde_binary <- EBImage::erode(kde_binary, kern)    
    kde_binary <- EBImage::dilate(kde_binary, kern)
    
    
    kdebin[,,i] <- kde_binary
    kde[,,i] <- kde_temp$fhat
    }
    
    if(length(which(ch==ch_range[i]))==0){
    warning(paste("Channel",ch_range[i],"is empty or has an error", sep=" "))
    kdebin[,,i] <- 0
    kde[,,i] <- 0
    }
  }
  
  if(output=="tiff"){
    
    if(length(ch_range)>3){stop("Tiff can only output up to 3 channels")}
    
    if(is.null(file)){file <- file.choose(new=TRUE)}
        
    if(length(ch_range)<=3){
      kde_tiff <- array(data=0, dim=(c(input_xsize,input_ysize,3)))
      kde_bin_tiff <- array(data=0, dim=(c(input_xsize,input_ysize,3)))
      for(i in 1:length(ch_range)){
        kde_tiff[,,i]<-EBImage::normalize(kde[,,i],ft=c(0,1))
        kde_bin_tiff[,,i] <- EBImage::normalize(kdebin[,,i],ft=c(0,1))
      }
      
      kde_tiff <- aperm(kde_tiff, c(2,1,3))
      kde_bin_tiff <- aperm(kde_bin_tiff, c(2,1,3))
      
      writeTIFF(kde_tiff, 
                paste(unlist(strsplit(file,"[.]"))[1],unlist(strsplit(file,"[.]"))[2],sep="_kde."), 
                bits.per.sample=8,
                compression = "none", reduce = T) 
      writeTIFF(kde_bin_tiff, 
                paste(unlist(strsplit(file,"[.]"))[1],unlist(strsplit(file,"[.]"))[2],sep="_kde_bin."), 
                bits.per.sample=8,
                compression = "none", reduce = T)   
    }
  }
  
  
  return(list(kde=kde, kde_binary=kdebin))
  
}

SMOLR_KDE <- function(x,y,ch,prec,bandwidth,xlim,ylim, px, threshold, file, output, fit){
  UseMethod("SMOLR_KDE")
}

SMOLR_KDE.default <- function(x,y,ch=NULL,prec=NULL, bandwidth= c(20,20),  xlim=NULL, ylim=NULL, px=5, threshold=0.05, file=NULL, output=c("r","tiff"), fit = TRUE){
  
  getkde <- function(x,y){return(y[x[1],x[2],x[3]])}
       
  if(is.null(xlim)==FALSE) {
    
    x_corr <- x-(floor(xlim[1])-px)
  }  
  if(is.null(ylim)==FALSE) {
    
    y_corr <- y-(floor(ylim[1])-px)
  } 
  if(is.null(xlim)){
    
    x_corr <- x - (floor(min(x))-px)
    xlim <- c(min(x),max(x))
  }
  if(is.null(ylim)){
   
    y_corr <- y - (floor(min(y))-px)
    #ylim <- c(0,max(y_corr))
    ylim <- c(min(y),max(y))
  }
  
  if(is.null(ch)){ch <- rep(1,length(x))}
  
  if(fit==TRUE){
    
    selection <- x>=xlim[1] & x<=xlim[2] & y>=ylim[1] & y<=ylim[2]
    #if(is.null(xlim) | is.null(ylim)){selection <- dx>=min(dx) & dx<=max(dx) & y>=min(y) & y<=max(y)}
    x <- x[selection]
   # dx_corr <- dx_corr[selection]
    y <- y[selection]
   #y_corr <- y_corr[selection]
    prec <- prec[selection]
    ch <- ch[selection]
    
  }
  
  ch_range <- unique(ch)
  
  img <- smolr_kde(x,y,ch,prec,bandwidth,xlim,ylim, px, threshold, file, output, fit)

  parameters <- SMOLR_PARAMETER(x,y,ch,prec,ch_range)
    
  intensities <- data.frame(
                        cbind(ch,
                              apply(cbind(trunc(x_corr/px),trunc(y_corr/px),sapply(ch,function(x) which(ch_range==x))),1,getkde,y=img$kde),
                              apply(cbind(trunc(x_corr/px),trunc(y_corr/px),sapply(ch,function(x) which(ch_range==x))),1,getkde, y=bwlabel(img$kde_binary))
                              )
                        )
  
  names(intensities) <- c("channel","kde_intensity","binary_no")
  clust_parameters <- data.frame(matrix(ncol=12,nrow = 1))[-1,]
  
  for(i in 1:length(ch_range)){
    if(length(which(ch==ch_range[i]))>0){
      for(j in 0:max(intensities$binary_no[intensities$channel==ch_range[i]])){
        clust_parameters_temp <- cbind(SMOLR_PARAMETER(x[intensities$channel==i&intensities$binary_no==j],y[intensities$channel==i&intensities$binary_no==j],ch[intensities$channel==i&intensities$binary_no==j],prec[intensities$channel==i&intensities$binary_no==j]),binary_no=j)
        if(j==0&i==1){names(clust_parameters) <- names(clust_parameters_temp)}
        clust_parameters <- rbind(clust_parameters,clust_parameters_temp)
      }
    }
  }
  
  inputs <- list(bandwidth=bandwidth,xlim=xlim,ylim=ylim, px=px, threshold=threshold, file=file, output=output, fit=fit, ch_range=ch_range)
  
  img <- c(img,parameters=list(parameters),int=list(intensities),clust_parameters=list(clust_parameters),inputs=list(inputs))
  
  class(img) <- "smolr_kde"
  return(img)
}

SMOLR_KDE.data.frame <- function(x,y=NULL,ch=NULL,prec=NULL, bandwidth= c(20,20),  xlim=NULL, ylim=NULL, px=5, threshold=0.05, file=NULL, output=c("r","tiff"), fit = TRUE){
    
  getkde <- function(x,y){return(y[x[1],x[2],x[3]])}
    
  ind_x <- grep("^x$",names(x),ignore.case=T)
  ind_y <- grep("^y$",names(x),ignore.case=T)
  ind_ch <- grep("^ch",names(x),ignore.case=T)
  ind_prec <- grep("^prec",names(x),ignore.case=T)
  
  dx <- x[,ind_x]
  y <- x[,ind_y]
  ch <- x[,ind_ch]
  prec <- x[,ind_prec]
  
  img <- SMOLR_KDE(x=dx,y=y,ch=ch,prec=prec, bandwidth= bandwidth,  xlim=xlim, ylim=ylim, px=px, threshold=threshold, file=file, output=output, fit = fit)
  
  class(img) <- "smolr_kde"
  return(img)
  
  
}

SMOLR_KDE.list <- function(x,y=NULL,ch=NULL,prec=NULL, bandwidth= c(20,20),  xlim=NULL, ylim=NULL, px=5, threshold=0.05,  file=NULL, output=c("r","tiff"), fit = TRUE){

  kde <- list()
  if(is.null(nrow(xlim)) & is.null(nrow(ylim)) ){
    for(i in 1:length(x)){
      kde[[i]] <- SMOLR_KDE(x[[i]],y,ch,prec,bandwidth,xlim,ylim, px, threshold, file, output, fit)
    }
  }else{
  if(nrow(xlim)==length(x) & nrow(ylim)==length(x) ){
    for(i in 1:length(x)){
      kde[[i]] <- SMOLR_KDE(x[[i]],y,ch,prec,bandwidth,xlim=as.numeric(xlim[i,]),ylim=as.numeric(ylim[i,]), px, threshold, file, output, fit)
    }  
    
  }
  }
  
  return(kde)
}


print.smolr_kde <- function(x,...){
  cat("Kernel density estimation \n \n")
  cat("number of channels: \t", length(x[[3]]$channel), "\n \n")
  
  print(x$parameters)
}


plot.smolr_kde <- function(x,y,brightness=0,contrast=1,saturate=0, ...){
  
      
  oripar <-  par(pty='s',mfrow=c(2,length(x$inputs$ch_range)),mar=c(2.5,2.5,2.5,2.5),no.readonly = TRUE)
    for(i in 1:length(x$inputs$ch_range)){
      img <- x[[1]][,,i]
      
      #saturate
      max <- sort(img)[length(img)*(1-saturate)]  
      img <- img/max
      #brightness
      img <- img+brightness
      
      #contrast
      img <- img*contrast
      
      #cut below 0 and above 1
      img[img>1] <- 1
      img[img<0] <- 0
      
      image(x=seq(1,nrow(x[[1]][,,i])*x$inputs$px,length.out = nrow(x[[1]][,,i])),
            y=seq(1,ncol(x[[1]][,,i])*x$inputs$px,length.out = ncol(x[[1]][,,i])),
            z=img, 
            main=paste("KDE channel", x$inputs$ch_range[i], sep=" "), 
            col=grey.colors(2^16, start=0, end=1),
            xlab="",
            ylab=""
      )
    }
    for(i in 1:length(x$inputs$ch_range)){
      image(x=seq(1,nrow(x[[2]][,,i])*x$inputs$px,length.out = nrow(x[[2]][,,i])),
            y=seq(1,ncol(x[[2]][,,i])*x$inputs$px,length.out = ncol(x[[2]][,,i])),
            z=x[[2]][,,i], 
            main=paste("KDE binary channel", x$inputs$ch_range[i], sep=" "), 
            col=grey.colors(2^16, start=0, end=1),
            xlab="",
            ylab="",
            xlim=c(1,nrow(x[[2]][,,i])*x$inputs$px),
            ylim=c(1,ncol(x[[2]][,,i])*x$inputs$px),
            asp=1
      )
    }
  par(oripar)
}


