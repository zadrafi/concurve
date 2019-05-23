# Reverse Engineer Consonance Functions Using the Point Estimate and Confidence Limits

reveng<- function(point, LL, UL, measure = "default") {
  if(is.numeric(point) != TRUE){
    stop("Error: 'x' must be a numeric vector")
  }
  if(is.numeric(LL) != TRUE){
    stop("Error: 'y' must be a numeric vector")
  }
  if(is.numeric(UL) != TRUE){
    stop("Error: 'y' must be a numeric vector")
  }

  intrvls <- (1:10000)/10000
  z<- qnorm(1-intrvls/2)

 if(measure == "default") {
  se <- (UL/LL)/3.92
  LL<- lapply(z, FUN = function(i) point + (i*se))
  UL<- lapply(z, FUN = function(i) point - (i*se))
  df<-data.frame(do.call(rbind,LL), do.call(rbind,UL))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$lower.limit <- exp(df$lower.limit)
  df$upper.limit <- exp(df$upper.limit)
 }

 else if(measure == "log") {
  se <- log(UL/LL)/3.92
  logpoint <- log(point)
  logLL<- lapply(z, FUN = function(i) logpoint + (i*se))
  logUL<- lapply(z, FUN = function(i) logpoint - (i*se))
  df<-data.frame(do.call(rbind,logLL), do.call(rbind,logUL))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$lower.limit <- exp(df$lower.limit)
  df$upper.limit <- exp(df$upper.limit)
 }

  df$intrvl.level <- 1-intrvls
  df$pvalue <- 1-(1-intrvls)
  df$svalue <- -log2(df$pvalue)
  df<-head(df,-1)
  return(df)
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
