#' function for hc values

hcv <- function(sens, inp){
  dist <- mlr(sens, input=inp)
  dist2 <- ssd_fit_dists(dist,dists = "burrIII3")
  estimate<-signif(ssd_hc(dist2, c(1, 5, 10, 20))$est,2)
  percent<-c("99%", "95%", "90%", "80%")
  out<-data.frame(percent,prettyNum(estimate, drop0trailing = TRUE))
  colnames(out)<- c("percent", "estimate")
  out
}

