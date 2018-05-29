smolr_plot <- function(x,y,size=NULL,color=NULL, rev.size=FALSE, rev.color=FALSE,  xlim=NULL, ylim=NULL, px=5, grey=FALSE, fit=TRUE, clim=NULL, slim=NULL,alpha=0.5, overlay=NULL,contrast=1,color_scale=NULL,sortChannels=TRUE){
  
  if(!is.numeric(x)){stop("x values are not (all) numeric")}
  if(!is.numeric(y)){stop("y values are not (all) numeric")}
  if(length(which(is.na(x)))>0){stop("x values contain NAs")}
  if(length(which(is.na(y)))>0){stop("y values contain NAs")}
  
  
  if((is.null(xlim) || length(xlim)==2)==FALSE){stop("xlim should be a vector with two values")}
  if((is.null(ylim) || length(ylim)==2)==FALSE){stop("ylim should be a vector with two values")}
  if((is.null(clim) || length(clim)==2)==FALSE){stop("clim should be a vector with two values")}
  if((is.null(slim) || length(slim)==2)==FALSE){stop("slim should be a vector with two values")}
  
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
    size <- size[selection]
    color <- color[selection]
    x <- x[selection]
    y <- y[selection]
    
  }
  
  
  colorfactor <- is.factor(color)
  
  
  if(is.null(color)){color <- rep(1,length(x))}
  
  if(is.null(clim)){
#     if (colorfactor){
#       max.col <- length(levels(color))
#       min.col <- 0
#     } else {
    max.col <- max(color)
    min.col <- min(color)
   # }
  }
  
  if(is.null(slim)){
    max.size <- max(size)
    min.size <- min(size)
    mid.size <- min(size)+((max(size)-min(size))/2)
  }
  
  if(!is.null(clim)){
    max.col <- clim[2]
    min.col <- clim[1] 
  }
  
  if(!is.null(slim)){
    
    max.size <- slim[2]
    min.size <- slim[1]
    mid.size <- slim[1]+((slim[2]-slim[1])/2)
  }
  
  
  
  if(min.size!=max.size){
    size <-  (size-min.size)/(max.size-min.size)  #normalize between 0 and 1 
  }
  if(min.size==max.size){
    size <- rep(1,length(x))
  }
  #if (!colorfactor){
  if(min.col!=max.col){
    color <- (color-min.col)/(max.col-min.col) 
  }
  if(min.col==max.col){
    color <- rep(1,length(x))  
  }
  #}
  if(rev.size){
    temp.size <- min.size
    min.size <- max.size
    max.size <- temp.size
    
  }
  
  selection <- size>=0 & size<=1 & color>=0 & color<=1
  x <- x[selection]
  y <- y[selection]
  color <- color[selection]
  size <- size[selection]
  
  if(rev.size){
    size <- 1-size
  }
  
  size <- (size*px*3)+ px #normalize between px and 5*px
  
  
  
  if(!grey&&is.null(color_scale)){colors <- rainbow(100, start=0, end=2/6, alpha=alpha)}
  if(!grey&&!is.null(color_scale)){colors <- color_scale}
  if(grey&&is.null(color_scale)){colors <- grey.colors(100, alpha=alpha)}
  
  
  color <- trunc(color*(length(colors)-1))+1 #normalize to integer between 1 and 100
  
  
  if(rev.color){colors <- rev(colors)}
  par(xpd=T)
  #oripar <- par(xpd=T,mar=c(3,3,3,2.5),no.readonly = TRUE)
  
  if(!is.null(overlay)){
    if(class(overlay)=="smolr_image"){
      overlay$img<-aperm(overlay$img, c(2,1,3)) #flip image into the right orientation
      if(dim(overlay$img)[3]==1){
        img <- array(dim=c(dim(overlay$img)[1],dim(overlay$img)[2],3),  data = 0)
        img[,,1] <- overlay$img
        img[,,2] <- overlay$img
        img[,,3] <- overlay$img
      } else if(dim(overlay$img)[3]==2){
        img <- array(dim=c(dim(overlay$img)[1],dim(overlay$img)[2],3),  data = 0)
        img[,,1:dim(overlay$img)[3]] <- overlay$img
      } else if(dim(overlay$img)[3]==3){
        img <- overlay$img
      } else if(dim(overlay$img)[3]>3){
        stop("only 3 channels supported with SMOLR_PLOT overlay")
      }
    }else {
      img <- readTIFF(overlay)
    }
    img <- (img / max(img))*contrast
    img[img>1] <- 1
    plot(1:2, type='n',xlim= c(0,input_xsize*px),ylim=c(0,input_ysize*px),xlab="",ylab="")
    lim <- par()
    rasterImage(EBImage::flop(img), 0, 0, input_xsize*px, input_ysize*px)
    symbols(x,y,circles=size,fg=colors[as.numeric(color)],bg=colors[as.numeric(color)],xlim= c(0,input_xsize*px),ylim=c(0,input_ysize*px), inches=FALSE, xlab="", ylab="",add=T)
    
  } else {
    if (!colorfactor){
      symbols(x,y,circles=size,fg=colors[color],bg=colors[color],xlim= c(0,input_xsize*px),ylim=c(0,input_ysize*px), inches=FALSE, xlab="", ylab="",add=F)
    } else {
      symbols(x,y,circles=size,fg=as.numeric(color),bg=as.numeric(color),xlim= c(0,input_xsize*px),ylim=c(0,input_ysize*px), inches=FALSE, xlab="", ylab="",add=F)
    }
  }
  
  if(max.col!=min.col&!colorfactor){
    gradient.rect(((input_xsize*px)/20),((input_ysize*px)+(input_ysize*px*0.05)),((input_xsize*px)/3),((input_ysize*px)+(input_ysize*px*0.1)),col=colors,nslices=100)
    if(min.size>100){labelcmin <- as.character(round(min.col,digits=0))}
    if(min.size<100){labelcmin <- as.character(signif(min.col,digits=2))}
    if(min.size>100){labelcmax <- as.character(round(max.col,digits=0))}
    if(min.size<100){labelcmax <- as.character(signif(max.col,digits=2))}
    text(0,((input_ysize*px)+(input_ysize*px*0.075)), labels=labelcmin, col="black", pos=2, cex=0.8)
    text(((input_xsize*px)/3)+((input_xsize*px)/20),((input_ysize*px)+(input_ysize*px*0.075)), labels=labelcmax, col="black",pos=4, cex=0.8)
  }
  if(max.size!=min.size){
    draw.circle((input_xsize*px)*1.1,input_ysize*px*(4/6), radius=px, border="black")
    draw.circle((input_xsize*px)*1.1,input_ysize*px*(5/6), radius=2*px, border="black")
    draw.circle((input_xsize*px)*1.1,input_ysize*px, radius=4*px, border="black")
    if(min.size>100){labelmin <- as.character(round(min.size,digits=0))}
    if(min.size<100){labelmin <- as.character(signif(min.size,digits=2))}
    if(min.size>100){labelmid <- as.character(round(mid.size,digits=0))}
    if(min.size<100){labelmid <- as.character(signif(mid.size,digits=2))}
    if(min.size>100){labelmax <- as.character(round(max.size,digits=0))}
    if(min.size<100){labelmax <- as.character(signif(max.size,digits=2))}
    text( (input_xsize*px)+(5*px),input_ysize*px*(3.5/6),labels=labelmin, col="black",pos=4, cex=0.8)
    text( (input_xsize*px)+(5*px),input_ysize*px*(4.5/6),labels=labelmid, col="black",pos=4, cex=0.8)
    text( (input_xsize*px)+(5*px),input_ysize*px*(5.5/6),labels=labelmax, col="black",pos=4, cex=0.8)
  }
}

SMOLR_PLOT <- function(x,y,size,color,rev.size,rev.color, xlim, ylim,px,grey,split_ch,fit,clim,slim,alpha,overlay,contrast,color_scale,sortChannels){
  UseMethod("SMOLR_PLOT")
}

SMOLR_PLOT.default <- function(x,y,size=NULL,color=NULL, rev.size=FALSE, rev.color=FALSE,  xlim=NULL, ylim=NULL, px=5, grey=FALSE,split_ch=FALSE, fit=TRUE, clim=NULL, slim=NULL,alpha=0.5, overlay=NULL,contrast=1,color_scale=NULL,sortChannels=TRUE){
  
  if(is.null(color)){color <- rep(1,length(x))}
  if(is.null(size)){size <- rep(1,length(x))}
#   if(fit==TRUE){
#     selection <- x>xlim[1] & x<xlim[2] & y>ylim[1] & y<ylim[2]
#     x <- x[selection]
#     y <- y[selection]
#     color <- color[selection]
#     size <- size[selection]
#   } 
  par(mfrow=c(1,1),pty="s",xpd=T)  
  smolr_plot(x,y,size,color,rev.size,rev.color, xlim, ylim,px,grey,fit,clim,slim,alpha,overlay,contrast,color_scale,sortChannels)
}

SMOLR_PLOT.data.frame <- function(x,y,size=NULL,color=NULL, rev.size=FALSE, rev.color=FALSE,  xlim=NULL, ylim=NULL, px=5, grey=FALSE,split_ch=FALSE, fit=TRUE, clim=NULL, slim=NULL, alpha=0.5,overlay=NULL,contrast=1,color_scale=NULL,sortChannels=TRUE){
  
  
  
  ind_x <- grep("^x$",names(x),ignore.case=T)
  ind_y <- grep("^y$",names(x),ignore.case=T)
  ind_ch <- grep("^ch",names(x),ignore.case=T)
  ind_prec <- grep("^prec",names(x),ignore.case=T)
  
  if(length(c(ind_x,ind_y,ind_ch,ind_prec))!=4){stop("Not all parameters (x,y,channel,precision) are present once in the header")}
  
  dx <- x[,ind_x]
  y <- x[,ind_y]
  ch <- x[,ind_ch]
  prec <- x[,ind_prec]
  
  if(is.null(xlim)){
    xlim <- c(min(dx),max(dx))
  }
  if(is.null(ylim)){
    ylim <- c(min(y),max(y))
  }
  
  range_ch <- unique(ch)
  
  if(sortChannels){
    ch_range <- sort(ch_range,decreasing = FALSE)
  }
  
  
  if(fit==TRUE){
    selection <- dx>=xlim[1] & dx<=xlim[2] & y>=ylim[1] & y<=ylim[2]
    dx <- dx[selection]
    y <- y[selection]
    prec <- prec[selection]
    ch <- ch[selection]
    range_ch <- unique(ch)
    if(sortChannels){
      ch_range <- sort(ch_range,decreasing = FALSE)
    }
    
  }
  
  if(is.null(clim) & is.null(color) & split_ch){clim = c(range_ch[1],range_ch[length(range_ch)])}
  if(is.null(color)){color <- ch}
  if(is.null(size)){size <- prec}
  if(length(size)==1){if(size=="off"){size <- rep(1,length(dx))}}
  if(length(unique(c(length(dx),length(y),length(color),length(size))))!=1){stop("size or color not of same length as x an y")}
  
  if(!split_ch){
    par(mfrow=c(1,1),pty="s",xpd=T)  
    smolr_plot(dx,y,size,color,rev.size,rev.color, xlim, ylim,px,grey,fit,clim,slim,alpha,overlay,contrast,color_scale,sortChanels)
  }
  
  if(split_ch){
    par(mfrow=c(1,length(range_ch)),pty="s",xpd=T)
    for(i in 1:length(range_ch)){
      smolr_plot(dx[ch==range_ch[i]],y[ch==range_ch[i]],size[ch==range_ch[i]],color[ch==range_ch[i]],rev.size,rev.color, xlim, ylim,px,grey,fit,clim,slim,alpha,overlay,contrast,color_scale,sortChannels)
    }
  }
}

