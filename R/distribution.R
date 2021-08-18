#' Function that fits ssd to burrIII distribution
#' Plot end result

fitSSD <- function(sens, input, hcv=NULL){
  Conc <- mlr(sens, input=input)
  dists <- ssd_fit_dists(Conc, dists = c("burrIII3"))
  p <- mutate(sens, Conc)
  preds <- predict(dists)
  ssd_plot(p, preds, left = "Conc", hc=hcv, label="PlotLabel", shift_x = 1.1)  + 
    scale_x_continuous(breaks=breaks_log())+ theme_classic() +
    xlab("Nickel, micrograms per litre") + ylab("Species Affected")
}