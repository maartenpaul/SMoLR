#multicell histogram
#input: a list of values

multicell_histogram <- function(x,var,breaks=NULL,error="sem",line=F,intercept=0){
  #determine limits
  data <- llply(x,function(x) data.frame(x[[var]],x[["condition"]]))
  alldata <- ldply(data)[,-1]
  mindata <- min(alldata[,1])-1
  maxdata <- max(alldata[,1])+1
  if(is.null(breaks)){
    breaks <- 20
  }
  limits <- seq(mindata,maxdata,(maxdata-mindata)/breaks)
  
  histdata <- llply(data,function(x) hist(x[,1],breaks = limits,plot = F))
  
  counts <- ldply(histdata, function(x) x$counts)
  mids <- histdata[[1]]$mids
  #counts$.id <- ldply(strsplit(counts$.id,split = "[:alnum:]"))[,1]
  counts$.id <- gsub(".$", "",counts$.id)
  #normalize to 1
  mean <- daply(counts,.variables = ".id", function(x) {
   x <- x[,-1]
     x <- x/rowSums(x)
    mean <- colMeans(x)
    
  }
    )
  
  sd <- daply(counts,.variables = ".id", function(x) {
    x <- x[,-1]
    x <- x/rowSums(x)
    sd <- sapply(x, function(x) sd(x)/sqrt(length(x)))
    
  }
  )
  
  library(reshape2)
  
  mean <- melt(mean)
  sd <- melt(sd)
  
  
  # if(error=="sem"){
  # sd <- sapply(counts, function(x) sd(x)/sqrt(length(x)))
  # } else {
  #   sd <- sapply(counts, function(x) sd(x))  
  # }
  bla <- data.frame(".id"=mean$.id,"x"=rep(mids,each=3),'mean'=mean$value,'se'=sd$value)
  bla$.id <- factor(bla$.id,
                 # levels = c("WT","FKA","FKL","KO"))
                  levels = c("WTR","151A","KO"))
  if (line){
  q1 <- ggplot(bla, aes(x=x, y=mean,colour=.id,fill=.id)) +
    geom_line(stat="identity")+
    geom_ribbon(aes(ymin=mean-se, ymax=mean+se,colour=NULL), alpha=0.5)+
    theme(text = element_text(size=20))+facet_grid(.id~.)+ geom_vline(aes(xintercept=intercept),color = "red",linetype="dashed") 
  #q1
  #q1
  } else {
  q1 <- ggplot(bla, aes(x=x, y=mean)) +
    geom_bar(position=position_dodge(width = 1.1), stat="identity")+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.2)+
    
    theme(text = element_text(size=20))+facet_grid(.id~.)+ geom_vline(aes(xintercept=intercept),color = "red",linetype="dashed") 
  }
  invisible(q1)
}
#load("Y:/Maarten/Zhanmin for Maarten/STORM analysis/Homer3N/eps45MinPts15_all_results _gaussfit.Rdata")
#data <- subset(all_results,condition=="WT")
#data <- split(all_results,f=all_results$condition)
#data <- split(all_results,f=all_results$cellID)
#str(data)

#dat <- multicell_histogram(data,"sd",breaks = 50,error="sem",line=F)
#dat

#dat + theme_classic()
# 
# data <- data.frame(bla[,1:2])
# # names(data) <- c("x","y")
# 
# 
# 
# fit <- nls(y ~ (C1 * exp(-(x-mean1)**2/(2 * sigma1**2)) +
#                   C2 * exp(-(x-mean2)**2/(2 * sigma2**2))),data = data,
#            start=list(C1=2, mean1=-2, sigma1=0.1,
#                       C2=2, mean2=1, sigma2=0.1), algorithm="port")
# coef(fit)
# plot(data)
# dat <- data.frame(x = seq(from = -3.8,to = 4.0,by = 0.05))
# lines(x = dat[,1],y=predict(fit,newdata = dat),col="red")
# #calcualte threshold
# threshold1 <- coef(fit)[2]+5*coef(fit)[3]
# abline(v = threshold1,col="red")


