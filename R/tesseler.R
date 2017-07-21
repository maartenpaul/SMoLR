smolr_tesseler <- function(x,y,ch=NULL, prec=NULL, eps = 50, MinPts=50){
  
  #convert to point pattern
  localizations_ppp <- SMOLR_TO_PPP(x,marks=x$Channel)
  
  #make tesselation
  tessel <- (deldir(localizations_ppp))

  #get neighbouring points
  ans <- tessel$delsgs[,5:6]
  ans <- data.frame("ind1"=c(ans[,1],ans[,2]),"ind2"=c(ans[,2],ans[,1]))
  ans <- ans[order(ans[,1]),]
  
  #calculate areas of tiles and apply threshold
  ans2 <- data.frame("area1"=tessel$summary$dir.area)
  ans2$area2 <- daply(ans,.variables = "ind1", function(x) sum(tessel$summary$dir.area[x[,2]]))
  ans2$density2 <- 1/ans2$area2
  mean_density <- nrow(ans2)/tessel$dir.area
  ans2$threshold <- ans2$density2>1*mean_density
  table(ans2$threshold)
  
  
  ans2 <- cbind(tessel$summary[,1:2],ans2)
  
  ans <- ans[is.element(ans[,1],which(ans2$threshold)),]
  ans2$.id <- seq(1:nrow(ans2))
  ans2 <- subset(ans2,ans2$threshold)
  
  
  ans3 <- ans
  ans2$cluster=-1
  stop <- FALSE
  total <- nrow(ans2)
  clusterid <- 1
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  ptm <- proc.time()
  while(!stop){
    Sys.sleep(0.1)
    # update progress bar
    if(any(ans2$cluster==-1)){
      k <- which(ans2$cluster==-1)[1]
      setTxtProgressBar(pb, total-length(which(ans2$cluster==-1)))
      if(nrow(ans[ans$ind1==ans2$.id[k],])==0){
        ans2$cluster[k]<-0 
      } else {
        
        incluster <- ans2$.id[k]
        incluster_old <- numeric(0)
        
        while(length(incluster)>length(incluster_old)){
          incluster_old <- incluster
          
          incluster <- unique(c(incluster,ans[is.element(ans[,1],incluster),2]))
          ans <- ans[is.element(ans[,1],ans2$.id[ans2$cluster==-1]),]
          
        }
        
        ans2$cluster[is.element(ans2$.id,incluster)] <- clusterid
        clusterid <- clusterid+1
        #ans3 <- ans3[is.element(ans3[,1],ans2$.id[ans2$cluster==-1]),]
      }
      
      
      
    } else {
      break
    }
  }
  
  x$cluster <- 0
  x[ans2$.id,]$cluster <- ans2$cluster
  
  return(x)
} 