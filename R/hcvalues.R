#' function for hc values

hcv <- function(input){
  dist <- zincMLR(input)
  dist2 <- ssd_fit_dists(dist,dists = "burrIII3")
  percent<-ssd_hc(dist2, c(1, 5, 10, 20))$percent
  estimate<-round(ssd_hc(dist2, c(1, 5, 10, 20))$est,2)
  cbind(percent,estimate)
}

