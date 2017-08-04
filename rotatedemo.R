
library(SMoLR)
library(EBImage)
library(plyr)

#make kernel density estimations from the data, and calculate features

pixelsize <- 5

test_kde <- SMOLR_KDE(smolrdata2, px=pixelsize, xlim=c(0,1000),ylim=c(0,1000))
test_features <- SMOLR_FEATURES(test_kde,"x.0.s.area",500)

plot(test_kde[[1]])
plot(test_kde[[2]])
plot(test_kde[[3]])


rotated_data <- list()

#do for al items in the list

for(i in 1:length(test_kde)){
  
  
  #determine filtered feature
  
  id <- test_features[[i]]$parameters$filtered_features_id
  id <- as.numeric(id)
  
  #determine center of filtered cluster
  
  x <- test_features[[i]]$channel_1[id,"x.0.m.cx"]*pixelsize
  y <- test_features[[i]]$channel_1[id,"x.0.m.cy"]*pixelsize
  
  #determine center of non filtered cluster
  
  x2 <- test_features[[i]]$channel_1[-id,"x.0.m.cx"]*pixelsize
  y2 <- test_features[[i]]$channel_1[-id,"x.0.m.cy"]*pixelsize
  
  #determine angle between two clusters
  
  angle <- atan2(y-y2,x-x2)
  
  #rotate around the center of the first cluster so that the second cluster is on top
  #translate so that the cluster is at coordinate 400,300
  
  rotated_data[[i]] <- SMOLR_ROTATE(smolrdata2[[i]],center=c(x,y) ,type = "radial", angle = angle+(pi/2), translate = c(400-x,300-y))
  
  
}
#plot rotated images

plot(SMOLR(rotated_data[[1]],xlim=c(0,800), ylim=c(0,800)))
plot(SMOLR(rotated_data[[2]],xlim=c(0,800), ylim=c(0,800)))
plot(SMOLR(rotated_data[[3]],xlim=c(0,800), ylim=c(0,800)))

#add all localizations to single list to make a summed image

summed_rotated_data <- ldply(rotated_data)

#plot the summed image

plot(SMOLR(summed_rotated_data,xlim=c(0,800), ylim=c(0,800)))




