# two-sided t-tests

meanintervals<-function(x, y, data, paired = F, method = "default", replicates = 1000, steps = 10000) {
  if(is.numeric(x) != TRUE){
    stop("Error: 'x' must be a numeric vector")
  }
  if(is.numeric(y) != TRUE){
    stop("Error: 'y' must be a numeric vector")
  }
  if(is.data.frame(data) != TRUE){
    stop("Error: 'data' must be a data frame")
  }
  if(is.numeric(replicates) != TRUE){
    stop("Error: 'replicates' must be a numeric vector")
  }
  if(is.numeric(steps) != TRUE){
    stop("Error: 'steps' must be a numeric vector")
  }
  intrvls <- (0:steps)/steps
  if(method == "default") {
    results <- lapply(intrvls, FUN = function(i) t.test(x, y, data=data, paired = paired , conf.level=i)$conf.int[])
  } else if(method == "boot") {
    diff <- mean(x) - mean(y)
    if(paired) {
      diffs <- x-y
      boot_dist <- replicate(replicates,
                             expr = mean(diffs[sample(length(diffs), replace = T)])) - diff
    } else {
      boot_dist <- replicate(replicates,
                             expr = mean(sample(x, length(x), replace = T)) -
                               mean(sample(y, length(y), replace = T))) - diff
    }
    results <- lapply(intrvls, FUN = function(i) diff - quantile(boot_dist, probs = (1+c(i,-i))/2))
  }
  df<-data.frame(do.call(rbind,results))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$intrvl.level <- intrvls
  df$pvalue <- 1-intrvls
  df$svalue <- -log2(df$pvalue)
  df<-head(df,-1)
  return(df)
}

