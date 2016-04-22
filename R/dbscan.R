#function to download elki from the server, however something is going wrong with the downloaded .jar
# get_elki <- function(url=NULL){
#   destfile <- "elki.jar"
#   
#   if(is.null(url)){
#     url="http://elki.dbs.ifi.lmu.de/releases/release0.6.5~20141030/elki-bundle-0.6.5~20141030.jar"
#   }
#   download.file(url, destfile, method="internal",mode="w", quiet = FALSE, 
#                 cacheOK = FALSE,
#                 extra = getOption("download.file.extra"))
# }

elki_dbscan <- function(data, eps=50,MinPts = 50){
  
  if(file.exists("elki.jar")==FALSE){stop(paste("elki.jar required in R working directory (", getwd(),"), consult help files for details",sep=""))}
  
  
  data2 <- data.frame(data$X,data$Y)
  write.table(data2,"data.txt",sep="\t",row.names = FALSE,col.names=FALSE)
  
  z <- system(paste("java -cp elki.jar de.lmu.ifi.dbs.elki.application.KDDCLIApplication -algorithm clustering.DBSCAN -dbc.in ", file.path(getwd(),"data.txt")," -dbscan.epsilon ",eps," -dbscan.minpts ",MinPts,sep=""),intern=TRUE)
  
  #find noise
  noise <- grep("# Cluster: Noise",x=z)
  noise <- data.frame(position=noise,name="Noise")
  
  #find clusters
  y <- grep("Cluster: Cluster",x = z)
  if(!length(y)==0){
    y <- data.frame(position=y,name=ldply(strsplit(z[y], ": "))[,2])
    y <- rbind(noise,y)
  
    y <- y[order(y[,1]),]
  } else {y<- noise}
  
  
  #get positions of Clusters
    
  y[,3] <- y[,1]+4
  if(nrow(y)>1){
  y[,4] <- c(y[2:nrow(y),1]-1,length(z))
  } else (y[,4] <- length(z))
  
  result <- alply(y,.margins=1,function(x) { i=as.numeric(x[3])
                                             j=as.numeric(x[4])
    return(z[i:j])})
  
  for (i in 1:length(result)){
    
    if(is.null(result[[i]])){
      result[[i]] <- NULL
    }
  }
  
  names(result) <- y[,2]
  
  #test <- result[[1]][1]
  
  result <- llply(result, function(x)  { 
    x<- t(apply(matrix(x),MARGIN = 1,FUN = function(x) {
      test <- strsplit(x,"=")
      test <- strsplit(test[[1]][2]," ")
      test <- test[[1]]
      test <- as.numeric(test)
      return(test)
    }))
      
  })
  
  result <- llply(result, function(x) {data.frame(x)})
  
  result <- ldply(result)

  result <-  result[order(result[,2]),c(3,4,1)]
  names(result) <- c("X","Y","Cluster")
  row.names(result) <- NULL
  
  result$Cluster[result$Cluster=="Noise"] <-0
  result$Cluster[result$Cluster=="Cluster"] <- 1
  
 if(length(result$Cluster[grep("Cluster ",result$Cluster)])!=0){
     result$Cluster[grep("Cluster ",result$Cluster)] <- as.numeric(ldply(strsplit(result$Cluster[grep("Cluster ",result$Cluster)]," "))[,2])+1
   }
   result$Cluster <- as.numeric(result$Cluster)
  
  return(na.omit(result))
  
}



#test
#test <- elki_dbscan(data = smlmrdata[smlmrdata$Channel==1,],eps=50,MinPts = 10)

smlmr_dbscan <- function(x,y,ch=NULL, prec=NULL, eps = 50, MinPts=50, xlim=NULL, ylim=NULL, fit = FALSE,elki=FALSE){
  
  
  if(is.null(ch)){ch <- rep(1,length(x))}
  if(is.null(prec)){prec <- rep(20,length(x))}
    ch_range <- unique(ch)
    
    dbscan_temp <- list()
  for(i in 1:length(ch_range)){
    if(elki==T){
      inp <- data.frame(X=x[ch==ch_range[i]],Y=y[ch==ch_range[i]],prec=prec[ch==ch_range[i]])
      k <- elki_dbscan(data = inp, eps=eps,MinPts = MinPts)
      k <- data.frame(inp[,1],inp[,2], inp[,3],k$Cluster)
      names(k) <- c("X","Y","Precision","Cluster")
      k$Channel <- ch_range[i]
      
      dbscan_temp[[i]] <- k
      
    } else {
      inp <- cbind(x[ch==ch_range[i]],y[ch==ch_range[i]],prec[ch==ch_range[i]])
      
      k <- dbscan::dbscan(x = matrix(inp[,1:2],ncol=2), eps=eps,minPts = MinPts)
      k <- data.frame(inp[,1],inp[,2], inp[,3],k$cluster)
      names(k) <- c("X","Y","Precision","Cluster")
      k$Channel <- ch_range[i]
      dbscan_temp[[i]] <- k
    }
   }

 
  
  return(dbscan_temp)
  
}


#test example:
#test <- smlmr_dbscan(x=smlmrdata[,1],y = smlmrdata[,2],ch=smlmrdata[,4],MinPts=10,elki=TRUE)


SMLMR_DBSCAN <- function(x,y,ch,prec, eps, MinPts, xlim, ylim, fit,elki=FALSE){
  UseMethod("SMLMR_DBSCAN")
}

SMLMR_DBSCAN.default <- function(x,y,ch=NULL, prec=NULL, eps = 50, MinPts=50,  xlim=NULL, ylim=NULL, fit = FALSE,elki=FALSE){
  
  if(is.null(ch)){ch <- rep(1,length(x))}
  if(is.null(prec)){prec <- rep(20,length(x))}
  
  ch_range <- unique(ch)
  
  dbscan_temp <- smlmr_dbscan(x,y,ch,prec,eps,MinPts,xlim,ylim, fit,elki)
  parameters <- SMLMR_PARAMETER(x,y,ch,prec)
    
#   intensities <- data.frame(cbind(ch,apply(cbind(trunc(x_corr/px),trunc(y_corr/px),sapply(ch,function(x) which(ch_range==x))),1,getkde,y=img$kde),apply(cbind(trunc(x_corr/px),trunc(y_corr/px),sapply(ch,function(x) which(ch_range==x))),1,getkde, y=bwlabel(img$kde_binary))))
#   names(intensities) <- c("channel","kde_intensity","binary_no")
  clust_parameters <- data.frame(matrix(ncol=12,nrow = 1))[-1,]
  for(i in 1:length(ch_range)){
    for(j in min(dbscan_temp[[ch_range[i]]][,4]):max(dbscan_temp[[ch_range[i]]][,4])){
    #for(j in 0:max(dbscan_temp[[i]][,3])){
      clust_parameters_temp <- cbind(SMLMR_PARAMETER(x[ch==ch_range[i]][dbscan_temp[[i]][,4]==j],
                                                     y[ch==ch_range[i]][dbscan_temp[[i]][,4]==j],
                                                     ch[ch==ch_range[i]][dbscan_temp[[i]][,4]==j],
                                                     prec[ch==ch_range[i]][dbscan_temp[[i]][,4]==j]),
                                                      binary_no=j)
      if(j==0&i==1){names(clust_parameters) <- names(clust_parameters_temp)}
      clust_parameters <- rbind(clust_parameters,clust_parameters_temp)
    }
  }
  
  inputs <- list(eps=eps,MinPts=MinPts,xlim=xlim,ylim=ylim, fit=fit,elki=elki)
  dbimg <- c(dbscan = list(dbscan_temp),parameters=list(parameters),clust_parameters=list(clust_parameters),inputs=list(inputs))
  
  class(dbimg) <- "smlmr_dbscan"
  return(dbimg)
}

#example
#k <- SMLMR_DBSCAN(x=smlmrdata[,1],y = smlmrdata[,2],prec = smlmrdata[,3],ch=smlmrdata[,4],eps = 100,MinPts=20)

SMLMR_DBSCAN.data.frame <- function(x,y,ch=NULL, prec=NULL, eps = 50, MinPts=50,  xlim=NULL, ylim=NULL, fit = FALSE,elki=FALSE){
  
  
  ind_x <- grep("^x$",names(x),ignore.case=T)
  ind_y <- grep("^y$",names(x),ignore.case=T)
  ind_ch <- grep("^ch",names(x),ignore.case=T)
  ind_prec <- grep("^prec",names(x),ignore.case=T)
  
  dx <- x[,ind_x]
  y <- x[,ind_y]
  ch <- x[,ind_ch]
  prec <- x[,ind_prec]
  
  #   if(is.null(xlim)==FALSE) {
  #     
  #     dx_corr <- dx-(floor(xlim[1])-px)
  #   }  
  #   if(is.null(ylim)==FALSE) {
  #     
  #     y_corr <- y-(floor(ylim[1])-px)
  #   } 
  #   if(is.null(xlim)){
  #         
  #     dx_corr <- dx - (floor(min(dx))-px)
  #     
  #   }
  #   if(is.null(ylim)){
  #     
  #     y_corr <- y - (floor(min(y))-px)
  #   }
  
  dbimg <- SMLMR_DBSCAN(dx,y,ch, prec, eps, MinPts,  xlim, ylim, fit,elki)
  

  return(dbimg)  
  
}


#k <- SMLMR_DBSCAN(x=smlmrdata,eps = 100,MinPts=20,elki=T)

SMLMR_DBSCAN.list <- function(x,y,ch=NULL, prec=NULL, eps = 50, MinPts=50,  xlim=NULL, ylim=NULL, fit = FALSE,elki=FALSE){
  
  
  
  dbscan_temp <- list()
  if(is.null(nrow(xlim)) & is.null(nrow(ylim)) ){
    for(i in 1:length(x)){
      dbscan_temp[[i]] <- SMLMR_DBSCAN(x[[i]],y,ch, prec, eps, MinPts,  xlim, ylim, fit,elki)
    }
  }else{
  if(nrow(xlim)==length(x) & nrow(ylim)==length(x) ){
    for(i in 1:length(x)){
      dbscan_temp[[i]] <- SMLMR_DBSCAN(x[[i]],y,ch,prec,eps,MinPts,xlim=as.numeric(xlim[i,]),ylim=as.numeric(ylim[i,]), fit,elki)
      
    }  
    
  }
  }
  
  return(dbscan_temp)
}

#k <- SMLMR_DBSCAN(testlist)

print.smlmr_dbscan <- function(x,...){
  cat("Density Based Spatial Clustering of applications with noise (DBSCAN) \n \n")
  cat("number of channels: \t", length(unique(x[[3]]$channel)), "\n \n")
  
  print(x$parameters)
}


plot.smlmr_dbscan <- function(x,y, ...){
  
  x <- ldply(x$dbscan,.id=NULL)
  
    if(max(x$Cluster)!=min(x$Cluster)){
    clim <-c(min(x$Cluster),max(x$Cluster))}
    else{
    clim <- NULL
    }
  
    SMLMR_PLOT(x = x,split_ch = T,color = x$Cluster, clim=clim, ...)
    
    
    
}

#plot(SMLMR_DBSCAN(x=smlmrdata,eps = 20,MinPts=5,elki=F))

