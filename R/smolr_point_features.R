smolr_point_features <- function(x){
  
    x <- x[x$Cluster>0,]
    out <- ddply(x,.variables = "dbscan",function(x) {
      
      D <- princomp(x[,2:3])
      
      angle <- atan2(D$loadings[2,1],D$loadings[1,1])
      
      # return(data.frame("N"=nrow(x),"sd"=((D$sdev[1]+D$sdev[2])/2)*2.35))
      return(
        data.frame("meanX" = mean(x$X),"meanY" = mean(x$Y),
                   "sd" = ((D$sdev[1] + D$sdev[2])/2) * 2.35,"width" = (max(D$scores[,1]) - min(D$scores[,1])),
                   "ratio" = (D$sdev[1] /D$sdev[2]),"angle" = angle,"N" = nrow(x)
        )
      )})
    
  }
  
