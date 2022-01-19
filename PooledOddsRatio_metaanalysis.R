
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
  
  #variance as the median variance in the effects across studies 
  
  
  Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset = rma(measure = "OR",  # "OR" for the log odds ratio in the metafor function 
                                                               yi = yi, 
                                                               vi = vi,
                                                               method = "REML", 
                                                               data=dat)
  
  #variance expressed as the hetoregeniety in teh effect (tau2)
  #summary_result = summary(Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset)
  #variance_Pooled_lOR = summary_result$tau2
 
  #variance as hetoregeneity in tau2 estimated from teh full model 
  #variance_Pooled_lOR = Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset$tau2.f

  
  #variance as tau2 from cumulative model 
  #cumul_MA_results = cumul(Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset, order=dat$Year)
  #variance_Pooled_lOR = cumul_MA_results$tau2
  
  LOGOdds_Ratio_result = predict(Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset)

  LOGOdds_Ratio = LOGOdds_Ratio_result$pred
  Standard_error_LogOR = LOGOdds_Ratio_result$se 
  LowerCI_LogOddsRatio = LOGOdds_Ratio_result$ci.lb
  UpperCI_LogOddsRatio = LOGOdds_Ratio_result$ci.ub
 
  k = Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset$k 
  
  #the mean samoling variance is set as teh variance for the likelihood distribution
  variance_Pooled_lOR = mean(dat$vi)
  
  # the mean samoling variance weighted by the number of studies 
  #variance_Pooled_lOR = mean(dat$vi)/k

  for (k in k){
    if(k == 1){LOGOdds_Ratio = likelihood_data_grouped$lOR
    }
    
  }
  
  # average Log Odds Ratio  
  


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