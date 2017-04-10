smolr_point_features <- function(x){
  #avoid issues with capital letters
    ind_cluster <- grep("^cluster$",names(x),ignore.case=T)
    ind_x <- grep("^x$",names(x),ignore.case=T)
    ind_y <- grep("^y$",names(x),ignore.case=T)
    ind_ch <- grep("^ch",names(x),ignore.case=T)
    ind_prec <- grep("^prec",names(x),ignore.case=T)
    
    x$cluster <- x[,ind_cluster]
    x <- x[x$cluster>0,]
    #go through the localizations and get statistics splitted by channel and cluster
    out <- ddply(x,.variables = c("Channel","cluster"),function(x) {
      
      dx <- x[,ind_x]
      y <- x[,ind_y]
      ch <- x[,ind_ch]
      prec <- x[,ind_prec]
      
      
      D <- princomp(cbind(dx,y))
      coord <- cbind(dx,y)
      angle <- atan2(D$loadings[2,1],D$loadings[1,1])
      
      chull_coord <- chull(coord)
      area <- pracma::polyarea(coord[rev(chull_coord),1], coord[rev(chull_coord),2])
      perimeter <- pracma::poly_length(coord[rev(chull_coord),1], coord[rev(chull_coord),2])
      
      
      xlim <- c(min(dx)-50,max(dx)+50)
      ylim <-  c(min(y)-50,max(y)+50)   
      kde <- SMOLR_KDE(x,threshold = 0.1,px=5,bandwidth = c(10,10),xlim=xlim,ylim=ylim)$kde_binary[,,1]
      
      skeleton <- thinImage(as.matrix(kde))
      #from pixels to nanometers    
      length_skeleton <- length(skeleton[skeleton==1])*5

    
      return(
        data.frame("meanX" = mean(dx),"meanY" = mean(y),
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
  x <- x$dbscan
  
  return(smolr_point_features(x))
}


SMOLR_POINT_FEATURES.list <- function(x){
  features <- list()
  llply(x,function(x){
    SMOLR_POINT_FEATURES(x)
  })
  
}




