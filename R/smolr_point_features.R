smolr_point_features <- function(x){
  #avoid issues with capital letters
    ind_cluster <- grep("^cluster$",names(x),ignore.case=T)
    x$cluster <- x[,ind_cluster]
    x <- x[x$cluster>0,]
    #go through the localizations and get statistics splitted by channel and cluster
    out <- ddply(x,.variables = c("Channel","cluster"),function(x) {
      
      D <- princomp(x[,2:3])
      coord <- x[,2:3]
      angle <- atan2(D$loadings[2,1],D$loadings[1,1])
      
      y <- chull(coord)
      area <- pracma::polyarea(coord[rev(y),1], coord[rev(y),2])
      perimeter <- pracma::poly_length(coord[rev(y),1], coord[rev(y),2])
      
      
      skeletonize <- function(x){
        sa <- matrix(1, nrow(x), ncol(x)) 
        skel <- matrix(0, nrow(x), ncol(x)) 
        kern <- makeBrush(3, shape="diamond") 
        while(max(sa)==1){
          k <- EBImage::opening(x, kern) 
          sa <- x-k 
          skel <- skel | sa
          x <- EBImage::erode(x, kern) 
        }
        return(skel)
      }
      
      xlim <- c(min(x$X)-50,max(x$X)+50)
      ylim <-  c(min(x$Y)-50,max(x$Y)+50)   
      kde <- SMOLR_KDE(x,threshold = 0.5,px=10,xlim=xlim,ylim=ylim)$kde_binary[,,1]
      image(kde)
      skeleton <- skeletonize(as.matrix(kde))
      image(skeleton)
      length_skeleton <- length(skeleton[skeleton==TRUE])*5

    
      # return(data.frame("N"=nrow(x),"sd"=((D$sdev[1]+D$sdev[2])/2)*2.35))
      return(
        data.frame("meanX" = mean(x$X),"meanY" = mean(x$Y),
                   "sd" = ((D$sdev[1] + D$sdev[2])/2) * 2.35,"width" = (max(D$scores[,1]) - min(D$scores[,1])),"area"=area,"perimeter"=perimeter,
                   "major_axis"= D$sdev[1]*2.35,"minor_axis"= D$sdev[2]*2.35 ,"ratio" = (D$sdev[1] /D$sdev[2]),"angle" = angle,"N" = nrow(x),"skeleton"=length_skeleton
        )
      )})
    
  }
  
SMOLR_POINT_FEATURES <- function(x){
  UseMethod("SMOLR_POINT_FEATURES")
}


SMOLR_POINT_FEATURES.default <- function(x){
  cat("x must be data.frame or of the class smolr_dbscan or smolr_kde")
}

SMOLR_POINT_FEATURES.data.frame <- function(x){
  x$Cluster <- 1
  smolr_point_features(x)
}

SMOLR_POINT_FEATURES.smolr_kde <- function(x){
  x <- x$int
  x$Cluster <- x$binary_no 
  x$Channel <- x$channel
  return(smolr_point_features(x))
}

SMOLR_POINT_FEATURES.smolr_dbscan <- function(x){
  x <- ldply(x$dbscan)
  
  return(smolr_point_features(x))
}


SMOLR_POINT_FEATURES.list <- function(x){
  features <- list()
  llply(x,function(x){
    SMOLR_POINT_FEATURES(x)
  })
  
}




