
library(dplyr)
library(metafor)



source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep=""))
source(paste(SOURCE_ROOT, "PooledN.R", sep=""))


metaDataLikelihood = function(likelihood_data, Construct, N) {
  index = likelihood_data$Construct == Construct
  likelihood_data_grouped = filter(likelihood_data, Construct == likelihood_data[index,]$Construct)
  


  dat = escalc(measure = "OR", 
         yi = likelihood_data_grouped$lOR, 
         vi = likelihood_data_grouped$varLOR)
  

  
  Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset = rma(measure = "OR",  
                                                               yi = yi, 
                                                               vi = vi,
                                                               method = "REML", 
                                                               data=dat)

  
  LOGOdds_Ratio_result = predict(Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset)

  LOGOdds_Ratio = LOGOdds_Ratio_result$pred
  Standard_error_LogOR = LOGOdds_Ratio_result$se 
  LowerCI_LogOddsRatio = LOGOdds_Ratio_result$ci.lb
  UpperCI_LogOddsRatio = LOGOdds_Ratio_result$ci.ub
 
  k = Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset$k 
  
  #the mean sampling variance is set as the variance for the likelihood distribution
  variance_Pooled_lOR = mean(dat$vi)
  

  for (k in k){
    if(k == 1){LOGOdds_Ratio = likelihood_data_grouped$lOR
    }
    
  }
  


  return(params = list(Construct = Construct, 
                       LOGOdds_Ratio = LOGOdds_Ratio, 
                       k = k, 
                       Standard_error_LogOR = Standard_error_LogOR, 
                       variance_Pooled_lOR = variance_Pooled_lOR, 
                       LowerCI_LogOddsRatio = LowerCI_LogOddsRatio, 
                       UpperCI_LogOddsRatio = UpperCI_LogOddsRatio))
}

Standard_deviation_LogOR = 0.3 + sqrt(19)
Variance_logOR = Standard_deviation_LogOR^2