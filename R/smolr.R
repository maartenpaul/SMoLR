### SMOLR_plot
### based on SMLM_plot v0.9 by Martijn de Gruiter Erasmus MC/ Erasmus OIC, Rotterdam, Netherlands
### minimum requirement is a set of coordinates


smolr <- function(x=NULL,y=NULL,prec=NULL,ch=NULL, px=5L,xlim = NULL, ylim = NULL,file=NULL, output=c("r","tiff"), fit=TRUE, fast=FALSE){
  
  
  
  if(is.null(ch)){ch=rep(1L,length(x))}
  ch_range <- unique(ch)
  if(is.null(prec)){prec=rep(20L,length(x))}
  if(is.null(x)||is.null(y)){stop("No x and y coordinates present")}
  if(length(unique(c(length(x),length(y),length(prec),length(ch))))!=1){stop("x, y, prec and ch differ in length")}
  if((is.null(xlim) || length(xlim)==2)==FALSE){stop("xlim should be a vector with two values")}
  if((is.null(ylim) || length(ylim)==2)==FALSE){stop("ylim should be a vector with two values")}
  if(!is.numeric(x)){stop("x values are not (all) numeric")}
  if(!is.numeric(y)){stop("y values are not (all) numeric")}
  if(!is.numeric(prec)){stop("precision values are not (all) numeric")}
  if(length(which(is.na(x)))>0){stop("x values contain NAs")}
  if(length(which(is.na(y)))>0){stop("y values contain NAs")}
  if(length(which(is.na(prec)))>0){stop("precision values contain NAs")}
  
  output <-match.arg(output)
  
  
  ## measure limits
  
  if(is.null(xlim)==FALSE) {
    input_xsize <- ((xlim[2]-xlim[1])/px)
    x <- x-xlim[1]
  }  
  if(is.null(ylim)==FALSE) {
    input_ysize <- ((ylim[2]-ylim[1])/px)
    y <- y-ylim[1]
  } 
  if(is.null(xlim)){
    input_xsize <- ((max(x)-min(x))/px)
    x <- x - min(x)
  }
  if(is.null(ylim)){
    input_ysize <- ((max(y)-min(y))/px)
    y <- y - min(y)
  }
  
  if(fit==TRUE){
    selection <- x>0 & x<input_xsize*px & y>0 & y<input_ysize*px
    x <- x[selection]
    y <- y[selection]
    prec <- prec[selection]
    ch <- ch[selection]
    
  } 
  
  
  
  if(max(x)>input_xsize*px||max(y)>input_ysize*px || min(x)<0 ||min(y)<0 ){stop("X or Y coordinates out of bound: use FIT=TRUE or set other xlim or ylim values")}
  
  
  pm<-ceiling(3*(max(prec)/px))
  pixx = input_xsize
  pixy = input_ysize
  xo <- ifelse(((x/px)%%1)>=0.5,((x/px)%%1L)-0.5, -1L*(0.5-((x/px)%%1)))
  yo <- ifelse(((y/px)%%1)>=0.5,((y/px)%%1L)-0.5, -1L*(0.5-((y/px)%%1)))
  sigx <- as.numeric(prec/px)    
  sigy <- as.numeric(prec/px)
  
  
  
  for(j in 1:length(ch_range)){
    
    ### create image matrix with extra borders for edge effects
    
    i_m<-matrix(data = 0L, 
                nrow = pixy+(4*pm), 
                ncol = pixx+(4*pm))  
    
    if(!fast){
      
      makeIMdata <- cbind(x[ch==ch_range[j]],y[ch==ch_range[j]],sigx[ch==ch_range[j]],sigy[ch==ch_range[j]],xo[ch==ch_range[j]],yo[ch==ch_range[j]])        
      
      
      ### create 2D gauss and add to image matrix
      makeIM <- function(i){      
        z <- GAUSS_FUNCTION(xo=i[5], yo=i[6], sigx=i[3], sigy=i[4], int_norm=TRUE)
        
        x1<-trunc(i[1]/px)+(2*pm)+1-floor(0.5*ncol(z))+1
        x2<-trunc(i[1]/px)+(2*pm)+1+floor(0.5*ncol(z))+1
        y1<-trunc(i[2]/px)+(2*pm)+1-floor(0.5*nrow(z))+1
        y2<-trunc(i[2]/px)+(2*pm)+1+floor(0.5*nrow(z))+1
        
        i_m[y1:y2, x1:x2]<<-i_m[y1:y2, x1:x2]+z
      }  
      
      apply(makeIMdata,1,FUN=makeIM)
      
    }
    
    
    if(fast){
      
      makeIMdata <- cbind(x[ch==ch_range[j]],y[ch==ch_range[j]],sigx[ch==ch_range[j]])
      
      
      makeIM <- function(i){
        
        
        z <- gausslist[[floor(i[3]*10)]]
        x1<-trunc(i[1]/px)+(2*pm)+1-floor(0.5*ncol(z))+1
        x2<-trunc(i[1]/px)+(2*pm)+1+floor(0.5*ncol(z))+1
        y1<-trunc(i[2]/px)+(2*pm)+1-floor(0.5*nrow(z))+1
        y2<-trunc(i[2]/px)+(2*pm)+1+floor(0.5*nrow(z))+1
        
        i_m[y1:y2, x1:x2]<<-i_m[y1:y2, x1:x2]+z
        
      }
      
      apply(makeIMdata,1,FUN=makeIM)
      
      
      
    }
    
    
    ### remove added edges    
    i_m<-i_m[ ((2*pm)+1):(nrow(i_m)-(2*pm)),
              ((2*pm)+1):(ncol(i_m)-(2*pm))]
    
    ### add image matrix to an array for return purposes 
    i_m<-array(i_m, c(nrow(i_m), ncol(i_m),1))
    if(j==1 && output=="r"){i_m_tif<-array(data=0, c(dim(i_m)[1], dim(i_m)[2],length(ch_range)))} 
    if(j==1 && output=="tiff"){i_m_tif<-array(data=0, c(dim(i_m)[1], dim(i_m)[2],3))} 
    i_m_tif[,,j]<-i_m_tif[,,j]+i_m[,,1]
    
  }
  
  ### output the array to R
  if(output=="r"){
    
    for(j in 1:length(ch_range)){
      ### Normalize between 0 and 1
      if(length(which(i_m_tif[,,j]=="NaN"))==0){
        i_m_tif[,,j]<-EBImage::normalize(i_m_tif[,,j], ft=c(0,1))
      }
      ### dont normalize if dataset is empty
      if(length(which(i_m_tif[,,j]=="NaN"))>0){
        i_m_tif[,,j] <- 0
        warning(paste("Channel",ch_range[j],"is empty or has an error", sep=" "))
      }
      
      ### Transpose so the image will be in R style x and y 
      i_m_tif[,,j] <- as.matrix(i_m_tif[,,j])
      
      
    }
    
    i_m_tif <- aperm(i_m_tif, c(2,1,3))
    
    return(i_m_tif)
  }
  
  
  ### output the array as a tiff
  if(output=="tiff"){
    
    
    
    ### automatic bit range selection
    bits <- 8
    
    for(j in 1:length(ch_range)) {
      ### test for empty or erronous arrays
      if(length(which(i_m_tif[,,j]=="NaN"))>0){
        
        i_m_tif[,,j] <- 0
        warning(paste("Channel",ch_range[j],"is empty or has an error", sep=" "))
      }
      
      if(max(i_m_tif[,,j])>2^bits && bits!=32){bits<-bits*2}
      if(max(i_m_tif[,,j])>2^bits && bits!=32){bits<-bits*2} ##repeat in case of initial 8 bit and 32bit needed
      if(max(i_m_tif[,,j])>2^bits && bits==32){cat("\nno larger bitrange available, saturated values are present in channel: ",j)}
      
      
    }
    
    ### normalize between 0 and 1 for the bitrange
    for(j in 1:length(ch_range)){
      i_m_tif[,,j]<-smolr_norm(i_m_tif[,,j],range=2^bits)
    }
    
    ### select a file is none was specified
    if(is.null(file)){file <- file.choose(new=TRUE)}
    
    ### write the tiff file
    writeTIFF(i_m_tif, 
              file, 
              bits.per.sample=bits,
              compression = "none", reduce = T) 
    
    filename <- paste("File was saved as",file,sep=":")
    return(filename)
  }
  
  
  
}

### Normalization function ###

smolr_norm <- function(x,range){
  
  x <- (x-0)/(range-0)
  
  return(x)
  
}


### Function to create a 2D or a 1D gaus with a given sigma in x and y and an offset

GAUSS_FUNCTION <- function(sigx,sigy=NULL,xo=0,yo=0, one_D=FALSE, int_norm=FALSE){
  
  if(is.null(sigy)){sigy <- sigx}
  
  gxx<-2*round(((0.34^-1)*sigx+.5)/2)
  gyy<-2*round(((0.34^-1)*sigy+.5)/2)
  gx <- seq((-1*gxx), gxx, length= (2*gxx+1))
  gy <- seq((-1*gyy), gyy, length= (2*gyy+1)) 
  if(one_D){gy <-0}
  gauss_sub<- function(gy,gx,xo,yo,sigx,sigy){
    part1 <- ((gx-xo)^2)/(2*(sigx^2)) 
    part2 <- ((gy-yo)^2)/(2*(sigy^2))
    gs <- exp(-(part1+part2))
    if(int_norm){gs <- (gs/sum(gs))*4050}
    return(gs)
  }
  z <- outer(gx,gy,gauss_sub, xo=xo, yo=yo,sigx=sigx, sigy=sigy)
  return(z)
  
}



SMOLR <- function(x,y,prec,ch, px,xlim,ylim,file,output,fit,fast){
  UseMethod("SMOLR")
}

SMOLR.default <- function(x,y,prec=NULL,ch=NULL, px=5,xlim = NULL, ylim = NULL,file=NULL, output=c("r","tiff"), fit=TRUE, fast=FALSE){
  
  if(is.null(ch)){ch <- rep(1, length(x))}
  if(is.null(prec)){prec <- rep(20, length(x))}
  
  
  
  img <- smolr(x,y,prec,ch,px,xlim,ylim,file,output,fit,fast)
  
  ch_range <- unique(ch)
  
  
  
  if(fit==TRUE){
    if(is.null(xlim)){xlim <- c(min(x),max(x))}
    if(is.null(ylim)){ylim <- c(min(y),max(y))}
    selection <- x>xlim[1] & x<xlim[2] & y>ylim[1] & y<ylim[2]
    x <- x[selection]
    y <- y[selection]
    prec <- prec[selection]
    ch <- ch[selection]
    
  }  
  
  
  inputs <- list(px=px,xlim=xlim,ylim=ylim,file=file,output=output,fit=fit,fast=fast, ch_range=ch_range)
  parameters <- SMOLR_PARAMETER(x,y,ch,prec,ch_range)
  img <- list(img=img, parameters=parameters, inputs=inputs )
  class(img) <- "smolr_image"
  return(img)
  
}

SMOLR.data.frame <- function(x,y=NULL,prec=NULL,ch=NULL, px=5,xlim = NULL, ylim = NULL,file=NULL, output=c("r","tiff"), fit=TRUE, fast=FALSE){
  
  ind_x <- grep("^x$",names(x),ignore.case=T)
  ind_y <- grep("^y$",names(x),ignore.case=T)
  ind_ch <- grep("^ch",names(x),ignore.case=T)
  ind_prec <- grep("^prec",names(x),ignore.case=T)
  
  dx <- x[,ind_x]
  y <- x[,ind_y]
  prec <- x[,ind_prec]
  ch <- x[,ind_ch]
  
  if(length(c(ind_x,ind_y,ind_prec,ind_ch))!=4){stop("Not all parameters (x,y,channel,precision) are present once in the header")}
  
  
  img <- SMOLR(x=dx,y=y,prec=prec,ch=ch,px=px,xlim=xlim,ylim=ylim,file=file,output=output,fit=fit,fast=fast)
  
  class(img) <- "smolr_image"
  return(img)
  
}

SMOLR.list <- function(x,y=NULL,prec=NULL,ch=NULL, px=5,xlim=NULL,ylim=NULL,file=NULL, output=c("r","tiff"), fit=TRUE, fast=FALSE){
  
  img <- list()
  if(is.null(nrow(xlim)) & is.null(nrow(ylim)) ){
    for(i in 1:length(x)){
      
      img[[i]] <- SMOLR(x[[i]],y,prec,ch,px,xlim,ylim,file,output,fit,fast)
    }
  }else{
    if(nrow(xlim)==length(x) & nrow(ylim)==length(x) ){
      for(i in 1:length(x)){
        
        img[[i]] <- SMOLR(x[[i]],y,prec,ch,px,xlim=as.numeric(xlim[i,]),ylim=as.numeric(ylim[i,]),file,output,fit)
      }
    }
  }
  return(img)
}


print.smolr_image <- function(x, ...){
  
  if(class(x[[1]])=="character"){ print(x[[1]]) }
  else{
    cat("single molecule image: \n \n")
    cat("Number of channels:\t",length(x$inputs$ch_range),"\n \n")
    
    
    print(x[[2]])
  }
}

plot.smolr_image <- function(x,y,saturate=0,brightness=0,contrast=1, rgb=F,...){
  
  if(class(x[[1]])=="character"){ print(x[[1]]) }
  else{
    if(rgb){
      x$img<-aperm(x$img, c(2,1,3)) #flip image into the right orientation
      x$img <- EBImage::flop(x$img)
      if(length(contrast==1)){
        contrast <- rep(contrast,length(x[[2]]$channel))
      }
      if(length(saturate==1)){
        saturate <- rep(saturate,length(x[[2]]$channel))
      }
      if(length(brightness==1)){
        brightness <- rep(brightness,length(x[[2]]$channel))
      }
      
      if(length(x[[2]]$channel)>3){stop("Maximum of three channels for RGB images")}
      
      for(i in 1:length(x[[2]]$channel)){
        img <- x$img[,,i]
        
        #saturate
        max <- sort(img)[length(img)*(1-saturate[i])]  
        img <- img/max
        #brightness
        img <- img+brightness[i]
        
        #contrast
        img <- img*contrast[i]
        
        #cut below 0 and above 1
        img[img>1] <- 1
        img[img<0] <- 0
        
        x$img[,,i] <- img
        
      }

      if(length(x[[2]]$channel)==1){
        col <- rgb(x[[1]][,,1],matrix(data = 0,ncol = ncol(x[[1]][,,1]),nrow=nrow(x[[1]][,,1]))
                   ,matrix(data = 0,ncol = ncol(x[[1]][,,1]),nrow=nrow(x[[1]][,,1])))
        
        dim(col) <- dim(x[[1]][,,1])
        
        
        #grid.raster(col, interpolate=FALSE)  
      }  
      
      if(length(x[[2]]$channel)==2){
        col <- rgb(x[[1]][,,1],x[[1]][,,2],matrix(data = 0,ncol = ncol(x[[1]][,,1]),nrow=nrow(x[[1]][,,1])))
        
        dim(col) <- dim(x[[1]][,,1])
      }  
      
      if(length(x[[2]]$channel)==3){
        col <- rgb(x[[1]][,,1],x[[1]][,,2],x[[1]][,,3])
        
        dim(col) <- dim(x[[1]][,,1])
      }  
      oripar <- par(mar=c(2.5,2.5,2.5,2.5),no.readonly = TRUE)
      plot(1:2, type='n',xlim= c(x$inputs$xlim[1],x$inputs$xlim[2]),ylim=c(x$inputs$ylim[1],x$inputs$ylim[2]), asp = 1,xlab="X",ylab="Y",bty="n",axes=FALSE, xaxs="i", yaxs="i")
      lim <- par()
      rasterImage(col, x$inputs$xlim[1], x$inputs$ylim[1], x$inputs$xlim[2], x$inputs$ylim[2])
      par(oripar)
    } else{
      
      oripar <- par(pty='s',mfrow=c(1,length(x$inputs$ch_range)),mar=c(2.5,2.5,2.5,2.5),no.readonly = TRUE)
      for(i in 1:length(x$inputs$ch_range)){
        img <- x$img[,,i]
        
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
        
        image(x=seq(1,dim(x$img[,,i])[1]*x$inputs$px,length.out = dim(x$img[,,i])[1]) ,
              y=seq(1,dim(x$img[,,i])[2]*x$inputs$px,length.out = dim(x$img[,,i])[2]) ,
              z=img, 
              main=paste("channel", x$inputs$ch_range[i], sep=" "), 
              col=grey.colors(2^16, start=0, end=1),
              xlab="",
              ylab="",
              xlim=c(1,dim(x$img[,,i])[1]*x$inputs$px),
              ylim=c(1,dim(x$img[,,i])[2]*x$inputs$px),
              asp=1,
              useRaster = T
        )
      }
      par(oripar)
    }
  }
}

plot_lut <- function(x,channel,lut){
  UseMethod("plot_lut")
}

plot_lut.smolr_image <- function(x,channel=1,lut="parula"){
  x$img <- x$img[,,channel]
  x$img<-aperm(x$img, c(2,1)) #flip image into the right orientation
  x$img <- EBImage::flop(x$img)
  # if(file.exists(file.path(path.package("SMolR"),paste0(lut,".tif")))){
  #   img <-readTIFF(file.path(path.package("SMolR"),paste0(lut,".tif")))
  # } else {
  #   stop("Lut file does not exists")
  # }
  #   
    #rgblut <- rgb(img[1,,1],img[1,,2],img[1,,3])
  rgblut <- get(lut)(256)
    D <- round(x$img*255)
    D2 <- matrix(nrow = nrow(D),ncol = ncol(D))
    for (i in 1:nrow(D)){
      for (j in 1:ncol(D)){
        D2[i,j] <- rgblut[D[i,j]+1]
      }
    }
    oripar <- par(mar=c(2.5,2.5,2.5,2.5),no.readonly = TRUE)
    plot(1:2, type='n',xlim= c(x$inputs$xlim[1],x$inputs$xlim[2]),ylim=c(x$inputs$ylim[1],x$inputs$ylim[2]), asp = 1,xlab="X",ylab="Y",bty="n",axes=FALSE)
    lim <- par()
    rasterImage(D2, x$inputs$xlim[1], x$inputs$ylim[1], x$inputs$xlim[2], x$inputs$ylim[2])
  }



