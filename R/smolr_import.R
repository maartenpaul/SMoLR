smolr_import <- function(folder=NULL,basename="",sep_chfiles=FALSE,channel=1,length_statistics=0,
                         profile=c("default","loc","roiloc","elyra","thunderstorm"),condition=NULL,remove_empty_ROI=FALSE,extension="txt",prename="",sep="\t",names=NULL){
  #should be compatible with Elyra .txt files, SMOLR Viewer .loc files and SMOLR ROI files (.txt)
  profile <- match.arg(profile)
  if(profile!="default"){
    switch(profile,
           loc={
             #basename <- ""
             sep <- "\t"
             extension <- "loc"
           },
           roiloc={
             basename <- "ROI_ch"
             sep <- "\t"
             extension <- "txt"
             length_statistics <- 8
             sep_chfiles <- TRUE
             
           },
           elyra={
             # basename <- ""
             sep <- "\t"
             extension<-"txt"
             names <- c("Index","First_Frame","Number_Frames", "Frames_Missing", "X","Y","Precision","Photons","Background","Fit_chi2","PSF_width","Channel","Z_slice")
             
           },
           thunderstorm={
             #  basename<-""
             names <- c("Index","First_Frame","X","Y","Z","sigma1","sigma2","Photons","Offset","Background","Fit","Precision","N_detections")
             sep <- ","
             extension<-"csv"
           })
  }
  
  files_to_list <- function(folder,basename,length_statistics,condition,extension,sep,names) {
    localizations <- list()
    files <- list.files(folder) 
    files <- files[grep(paste0(extension,"$"),files)]
    files <- files[grep(paste("^",basename,sep=""),files)]
    
    n <- length(files)
    if(length_statistics>0){
      statistics <- data.frame("CELL_id"=numeric(),"ROI_id"=numeric(),"ROI_width"=numeric(),"ROI_height"=numeric(),"ROI_xmin"=numeric(),"ROI_xmax"=numeric(),"ROI_ymin"=numeric(),"ROI_ymax"=numeric())
    }
    
    for (i in 1:n){
      if(basename==""){
        file <- file.path(folder,files[i])
        #file <- file.path(folder,files[i])
      } else{
        file <- file.path(folder,paste(basename,as.character(i),".",extension, sep="")) 
      }
      if (file.exists(file)){
        if (file.info(file)$size>0){
          
          locs_roi <- read.csv(file=file ,header =TRUE,sep=sep,stringsAsFactors=F)
          
          if(length_statistics>1){
            stat <- tail(locs_roi,length_statistics)
            #still needs to be made variable
            stat <- stat$X[2:7]
            
            locs_roi <-head(locs_roi,-length_statistics)
            statistics[i,2:8] <- c(i,as.numeric(stat))
            statistics[i,1] <- basename(folder)
            
            
          }
          if(nrow(locs_roi)!=0){
            if(!is.null(condition)){
             
              locs_roi$Condition <- condition
            }
            if(!is.null(names)){
              names(locs_roi) <- names
            }
            
            Locname <- paste(basename(folder),i,sep="_")
            localizations[[Locname]] <- na.omit(locs_roi) #remove rows with NA's 
            
            
          } else {
            Locname <- paste(basename(folder),i,sep="_")
            localizations[[Locname]] <- NA
          }
          
        } 
      }
    }
    if(length_statistics>1){
      for (i in 1:length(localizations)){
        attr(localizations[[i]],"statistics") <- statistics[i,]
      }
      if (n!=0){
        write.table(cbind(rowMeans(cbind(as.numeric(statistics$ROI_xmin),as.numeric(statistics$ROI_xmax))),
                          rowMeans(cbind(as.numeric(statistics$ROI_ymin),as.numeric(statistics$ROI_ymax)))),file = file.path(folder,"ROIstatistics.txt"),row.names = FALSE,col.names = FALSE)
      }
      return(list(localizations,statistics))
    } else {
      return(localizations)
    }
    
    
    
    
    
  }
  
  import_files <- function(folder,basename,sep_chfiles,channel,length_statistics,condition,extension,sep,names){
    if(sep_chfiles){
      if(length_statistics>0){
        if(basename==""){
          loc <- files_to_list(folder = folder,basename = basename,length_statistics = length_statistics,condition = condition,extension=extension,sep=sep,names=names)   
        } else {
          loc <- files_to_list(folder = folder,basename = paste(basename,channel[1],"_",sep=""),length_statistics = length_statistics,condition = condition,extension=extension,sep=sep,names=names)  
        }
        
        localizations <- loc[[1]]
        #if(nrow(localizations)/nrow(localizations[duplicated(localizations),])>0.4){stop(paste("Too many duplicated localizations in",folder,basename,channel[1],sep=" "))}
        statistics <- loc[[2]]
      } else {
        if(basename==""){
          localizations <- files_to_list(folder = folder,basename = basename,length_statistics,condition,extension=extension,sep=sep,names=names)  
        } else {
          localizations <- files_to_list(folder = folder,basename = paste(basename,channel[1],"_",sep=""),length_statistics,condition,extension=extension,sep=sep,names=names)  
        }
      }
      
      if (length(channel) > 1){
        for (i in 2:length(channel)){
          if(length_statistics>0){
            if(basename==""){
              localizations_tmp <- files_to_list(folder,basename=basename,length_statistics = length_statistics,condition,extension=extension,sep=sep,names=names)[[1]] 
            } else{
              localizations_tmp <- files_to_list(folder,paste(basename,channel[i],"_",sep=""),length_statistics,condition,extension=extension,sep=sep,names=names)[[1]] 
            }
            if(length(localizations)==length(localizations_tmp)){
              localizations <- mapply(rbind,localizations,localizations_tmp,SIMPLIFY=FALSE)
            } else {stop("Something different between the channels")}
          } else {
            if(basename==""){
              localizations_tmp <- files_to_list(folder,basename=basename,length_statistics,condition,extension=extension,sep=sep,names=names)
            }else{
              localizations_tmp <- files_to_list(folder,paste(basename,channel[i],"_",sep=""),length_statistics,condition,extension=extension,sep=sep,names=names)
              
            }
            if(length(localizations)==length(localizations_tmp)){
              localizations <- mapply(rbind,localizations,localizations_tmp,SIMPLIFY=FALSE)
            } else {stop("Something different between the channels")}
          }
          
          
        }
        
      }
      
    } else {
      
      localizations <- files_to_list(folder=folder,basename = basename,length_statistics = length_statistics,condition = condition,extension = extension,sep = sep,names=names)
      #       statistics <- localizations[[2]]
      #       localizations <- localizations[[1]]
    }
    if(length_statistics>1){
      for (i in 1:length(localizations)){
        attr(localizations[[i]],"statistics") <- statistics[i,]
      }
      return(list(localizations,statistics))
    } else {
      return(localizations)
    }
    
  }
  
  if (is.null(folder))  folder <- choose.dir("", "Choose a suitable folder")
  
  if(length(list.dirs(folder,recursive=F))==0) {
    localizations <- import_files(folder = folder,basename = basename,sep_chfiles = sep_chfiles,channel = channel,
                                  length_statistics = length_statistics,condition = condition,extension = extension,sep=sep,names=names)
    if (length_statistics>0){
      statistics <- localizations[[2]]
      localizations <- localizations[[1]]
    }
    
  } else if(length(list.dirs(folder,recursive=F))!=0) {
    
    localizations <- list()
    statistics <- list()
    directories <- list.dirs(folder,recursive=F)
    for (i in 1:length(directories)){
      localizations_tmp <- import_files(folder=directories[i],basename = basename,sep_chfiles = sep_chfiles,channel = channel,
                                        length_statistics = length_statistics,condition = condition,extension = extension,sep=sep,names=names)
      if(length_statistics>0){  
        statistics[[i]] <- localizations_tmp[[2]] 
        localizations_tmp <- localizations_tmp[[1]]
      } 
      localizations <- append(localizations,localizations_tmp) 
      rm(localizations_tmp)      
    }
    if(length_statistics>0){  
      statistics <- ldply(statistics)
    }
  }
  
  
  if(remove_empty_ROI==TRUE){
    numberofrows <- unlist(lapply(localizations,nrow))
    clean <- (numberofrows!=0&!unlist(lapply(localizations,function(x) any(is.na(x)))))
    localizations <- localizations[clean]
    statistics <- statistics[clean,] 
  }
  
  save(localizations,file=file.path(folder,paste(prename,"localizations.Rdata",sep="")))   
  if(length_statistics>1){
    save(statistics,file=file.path(folder,paste(prename,"statistics.Rdata",sep="")))
  }
  
}


SMOLR_IMPORT <- function(folder,basename,sep_chfiles,channel,length_statistics,profile,condition,remove_empty_ROI,extension,prename,sep,names){
  UseMethod("SMOLR_IMPORT")
}

SMOLR_IMPORT.default <- function(folder=NULL,basename="",sep_chfiles=FALSE,channel=1,length_statistics=0,
                                 profile=c("default","loc","roiloc","elyra","thunderstorm"),condition=NULL,remove_empty_ROI=FALSE,extension="txt",prename="",sep="\t",names=NULL){
  smolr_import(folder,basename,sep_chfiles,channel,length_statistics,profile,condition,remove_empty_ROI,extension,prename,sep,names)}
    
