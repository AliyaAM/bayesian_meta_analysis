
library(dplyr)
library(metafor)

x = read.csv(paste(SOURCE_ROOT, "input.csv", sep=""))  # requires this data for all indexed functions that read information for each construct across studies 


source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep=""))
source(paste(SOURCE_ROOT, "PooledN.R", sep=""))


metaDataLikelihood = function(likelihood_data, Construct, N) {
  index = likelihood_data$Construct == Construct
  likelihood_data_grouped = filter(likelihood_data, Construct == likelihood_data[index,]$Construct)

  dat = escalc(measure = "OR", 
         yi = likelihood_data_grouped$lOR, 
         vi = likelihood_data_grouped$varLOR)
  
  Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset = rma(measure = "OR",  # "OR" for the log odds ratio in the metafor function 
                                                               yi = yi, 
                                                               vi = vi,
                                                               method = "REML", 
                                                               data=dat)
  
  variance_across_studies = mean(dat$vi)
  
  summary(Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset)
  
  LOGOdds_Ratio_result = predict(Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset)

  LOGOdds_Ratio = LOGOdds_Ratio_result$pred
  Standard_error_LogOR = LOGOdds_Ratio_result$se 
  
  Standard_deviation_LogOR = Standard_error_LogOR + sqrt(N)
  
  LowerCI_LogOddsRatio = LOGOdds_Ratio_result$ci.lb
  UpperCI_LogOddsRatio = LOGOdds_Ratio_result$ci.ub
  k = Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset$k


  for (k in k){
    if(k == 1){LOGOdds_Ratio = likelihood_data_grouped$lOR
    }
    
  }
  
  # average Log Odds Ratio  
  


  return(params = list(Construct = Construct, 
                       LOGOdds_Ratio = LOGOdds_Ratio, 
                       k = k, 
                       variance_across_studies = variance_across_studies, 
                       Standard_error_LogOR = Standard_error_LogOR, 
                       Standard_deviation_LogOR = Standard_deviation_LogOR, 
                       LowerCI_LogOddsRatio = LowerCI_LogOddsRatio, 
                       UpperCI_LogOddsRatio = UpperCI_LogOddsRatio))
}

