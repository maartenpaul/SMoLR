apply_filtered_features <- function(features, kde){

  
if(class(kde[[1]])=="smlmr_kde"){    
for(i in 1:length(kde)){  

  kde[[i]]$clust_parameters$filtered <-FALSE
  
  for(j in 1:nrow(kde[[i]]$parameters)){
    
    ff <- as.numeric(unlist(strsplit(as.character(features[[i]]$parameters$filtered_features_id[j]), " ")))
    
    
    for(k in 1:length(ff)){
      
      j <- kde[[i]]$parameters$channel[j]
      rowno <- which(kde[[i]]$clust_parameters$channel==j & kde[[i]]$clust_parameters$binary_no==ff[k])
      kde[[i]]$clust_parameters$filtered[rowno] <- TRUE
      
      
    }
  }
}
}

  if(class(kde[[1]])=="array") {
    
    kde$clust_parameters$filtered <-FALSE
    
    for(j in 1:nrow(kde$parameters)){
      
      ff <- as.numeric(unlist(strsplit(as.character(features$parameters$filtered_features_id[j]), " ")))
      
      
      for(k in 1:length(ff)){
        
        j <- kde$parameters$channel[j]
        rowno <- which(kde$clust_parameters$channel==j & kde$clust_parameters$binary_no==ff[k])
        kde$clust_parameters$filtered[rowno] <- TRUE
        
        
      }
    }
  } 

return(kde)
}






