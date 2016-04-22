smlmr_load <- function(folder,statistics=F,prename=""){
  load(file.path(folder,paste(prename,"localizations.Rdata",sep="")),.GlobalEnv)
  if (statistics==T){
    load(file.path(folder,paste(prename,"statistics.Rdata",sep="")),.GlobalEnv)
    if(nrow(statistics)!=length(localizations)) warning("localizations and statistics not the same length")
  }
  
}

SMLMR_LOAD <- function(folder,statistics,prename){
  UseMethod("smlmr_load")
}

SMLMR_LOAD.default <- function(folder,statistics=F,prename=""){
  smlmr_load(folder,statistics,prename)
}