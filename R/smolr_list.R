smolr_list <- function(x,xlim=NULL,ylim=NULL){
  
  
  if(is.null(nrow(xlim))|is.null(nrow(ylim))){stop("not enough limits to make a list")}
  if(nrow(xlim)!=nrow(ylim)){stop("xlim and ylim not of same length")}
  
  ind_x <- grep("^x$",names(x),ignore.case=T)
  ind_y <- grep("^y$",names(x),ignore.case=T)
    
  dx <- x[,ind_x]
  y <- x[,ind_y]
  
  if(length(c(ind_x,ind_y))!=2){stop("Not all parameters (x,y) are present once in the header")}
    
  smolrlist <- list()
  
  for(i in 1:nrow(xlim)){
    rows <- which(dx>xlim[i,1] & dx<xlim[i,2] & y>ylim[i,1] & y<ylim[i,2])
    smolrlist[[i]] <- x[rows,]
  }

  return(smolrlist)

}

SMOLR_LIST <- function(x,xlim,ylim){
  UseMethod("SMOLR_LIST")
}

SMOLR_LIST.default <- function(x,xlim=NULL,ylim=NULL){
 cat("x must be a data.frame")
}

SMOLR_LIST.data.frame <- function(x,xlim=NULL,ylim=NULL){
  smolr_list(x,xlim,ylim)
}





