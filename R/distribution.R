#' Function that fits ssd to burrIII distribution
#' Plot end result


zincSSD <- function(input, hcv=NULL){
  Conc <- zincMLR(input)
  dists <- ssd_fit_dists(Conc, dists = c("burrIII3"))
  p<-mutate(zincdata, Conc)
  preds<-predict(dists)
  ssd_plot(p, preds,left="Conc", hc=hcv, shape="Species.Type") + theme_bw() + scale_x_continuous(breaks=breaks_log())+
    xlab("Concentration") + ylab("Species Affected")
}
