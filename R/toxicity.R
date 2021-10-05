#' Function that returns species toxicity
#' @export Conc

mlr <- function(sensitivity = nidata, tMLR = MLR, input){
  Conc <- vector(length = nrow(sensitivity))
  for(i in 1:nrow(sensitivity)){
    if(sensitivity[i,]$Species.Type=="Fish"){
      fish <- tMLR[tMLR$type=="Fish",]
      Conc[i] <- exp(sensitivity[i,]$Sensitivity + fish$DOC*log(input$DOC) + fish$Mg*log(input$Mg) +
                       fish$pH*input$pH + fish$DOC.pH*log(input$DOC)*input$pH +
                       fish$Mg.pH*log(input$Mg)*input$pH)
    } else if(sensitivity[i,]$Species.Type=="Invertebrate"){
      inv <- tMLR[tMLR$type=="Invertebrates",]
      Conc[i] <- exp(sensitivity[i,]$Sensitivity + inv$DOC*log(input$DOC) + inv$Ca*log(input$Ca)+
                       inv$Mg*log(input$Mg) + inv$pH*input$pH + inv$DOC.pH*log(input$DOC)*input$pH)
    } else if(sensitivity[i,]$Species.Type=="Microalgae"){
      alg <- tMLR[tMLR$type=="Algae",]
      Conc[i] <- exp(sensitivity[i,]$Sensitivity + alg$DOC*log(input$DOC) +
                       alg$pH*input$pH + alg$Mg*log(input$Mg))
    } else if(sensitivity[i,]$Species.Type=="Macrophyte"){
      aq <- tMLR[tMLR$type=="Aquatic Plants",]
      Conc[i] <- exp(sensitivity[i,]$Sensitivity + aq$DOC*log(input$DOC) + aq$pH*input$pH)
    }
  }
  as.data.frame(Conc)
}
