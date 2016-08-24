smolr_dbscan <- function(x,y,ch=NULL, prec=NULL, eps = 50, MinPts=50){
  
  if(is.null(ch)){ch <- rep(1,length(x))}
  if(is.null(prec)){prec <- rep(20,length(x))}
    ch_range <- unique(ch)
    
    dbscan_temp <- list()
  for(i in 1:length(ch_range)){
    
      inp <- cbind(x[ch==ch_range[i]],y[ch==ch_range[i]],prec[ch==ch_range[i]])
      
      k <- dbscan::dbscan(x = matrix(inp[,1:2],ncol=2), eps=eps,minPts = MinPts)
      k <- data.frame(inp[,1],inp[,2], inp[,3],k$cluster)
      names(k) <- c("X","Y","Precision","Cluster")
      k$Channel <- ch_range[i]
      dbscan_temp[[i]] <- k
   
   }

 
  
  return(dbscan_temp)
  
}


#test example:
#test <- smolr_dbscan(x=smolrdata[,1],y = smolrdata[,2],ch=smolrdata[,4],MinPts=10,elki=TRUE)


SMOLR_DBSCAN <- function(x,y,ch,prec, eps, MinPts){
  UseMethod("SMOLR_DBSCAN")
}

SMOLR_DBSCAN.default <- function(x,y,ch=NULL, prec=NULL, eps = 50, MinPts=50){
  
  if(is.null(ch)){ch <- rep(1,length(x))}
  if(is.null(prec)){prec <- rep(20,length(x))}
  
  ch_range <- unique(ch)
  
  dbscan_temp <- smolr_dbscan(x,y,ch,prec,eps,MinPts)
  parameters <- SMOLR_PARAMETER(x,y,ch,prec)
    
#   intensities <- data.frame(cbind(ch,apply(cbind(trunc(x_corr/px),trunc(y_corr/px),sapply(ch,function(x) which(ch_range==x))),1,getkde,y=img$kde),apply(cbind(trunc(x_corr/px),trunc(y_corr/px),sapply(ch,function(x) which(ch_range==x))),1,getkde, y=bwlabel(img$kde_binary))))
#   names(intensities) <- c("channel","kde_intensity","binary_no")
  clust_parameters <- data.frame(matrix(ncol=12,nrow = 1))[-1,]
  for(i in 1:length(ch_range)){
    for(j in min(dbscan_temp[[ch_range[i]]][,4]):max(dbscan_temp[[ch_range[i]]][,4])){
    #for(j in 0:max(dbscan_temp[[i]][,3])){
      clust_parameters_temp <- cbind(SMOLR_PARAMETER(x[ch==ch_range[i]][dbscan_temp[[i]][,4]==j],
                                                     y[ch==ch_range[i]][dbscan_temp[[i]][,4]==j],
                                                     ch[ch==ch_range[i]][dbscan_temp[[i]][,4]==j],
                                                     prec[ch==ch_range[i]][dbscan_temp[[i]][,4]==j]),
                                                      binary_no=j)
      if(j==0&i==1){names(clust_parameters) <- names(clust_parameters_temp)}
      clust_parameters <- rbind(clust_parameters,clust_parameters_temp)
    }
  }
  
  inputs <- list(eps=eps,MinPts=MinPts)
  dbimg <- c(dbscan = list(dbscan_temp),parameters=list(parameters),clust_parameters=list(clust_parameters),inputs=list(inputs))
  
  class(dbimg) <- "smolr_dbscan"
  return(dbimg)
}

#example
#k <- SMOLR_DBSCAN(x=smolrdata[,1],y = smolrdata[,2],prec = smolrdata[,3],ch=smolrdata[,4],eps = 100,MinPts=20)

SMOLR_DBSCAN.data.frame <- function(x,y,ch=NULL, prec=NULL, eps = 50, MinPts=50){
  
  
  ind_x <- grep("^x$",names(x),ignore.case=T)
  ind_y <- grep("^y$",names(x),ignore.case=T)
  ind_ch <- grep("^ch",names(x),ignore.case=T)
  ind_prec <- grep("^prec",names(x),ignore.case=T)
  
  dx <- x[,ind_x]
  y <- x[,ind_y]
  ch <- x[,ind_ch]
  prec <- x[,ind_prec]
  
  dbimg <- SMOLR_DBSCAN(dx,y,ch, prec, eps, MinPts)
  

  return(dbimg)  
  
}


#k <- SMOLR_DBSCAN(x=smolrdata,eps = 100,MinPts=20,elki=T)

SMOLR_DBSCAN.list <- function(x,y,ch=NULL, prec=NULL, eps = 50, MinPts=50){

  dbscan_temp <- llply(x,function(x){
          SMOLR_DBSCAN(x,y,ch, prec, eps, MinPts)
      })

  
  return(dbscan_temp)
}

#k <- SMOLR_DBSCAN(testlist)

print.smolr_dbscan <- function(x,...){
  cat("Density Based Spatial Clustering of applications with noise (DBSCAN) \n \n")
  cat("number of channels: \t", length(unique(x[[3]]$channel)), "\n \n")
  
  print(x$parameters)
}


plot.smolr_dbscan <- function(x,y, hide_noise=FALSE, ...){
  
  x <- ldply(x$dbscan,.id=NULL)
  
    if(max(x$Cluster)!=min(x$Cluster)){
      if(hide_noise){
        clim <-c(1,max(x$Cluster))
      } else{
        clim <-c(min(x$Cluster),max(x$Cluster))
      }
    }   else{
    clim <- NULL
    }
  
    SMOLR_PLOT(x = x,split_ch = T,color = x$Cluster,clim=clim, ...)
    
}

#plot(SMOLR_DBSCAN(x=smolrdata,eps = 20,MinPts=5,elki=F))

