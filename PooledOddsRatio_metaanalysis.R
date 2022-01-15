
library(dplyr)
library(metafor)

x = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/input.csv')  # requires this data for all indexed functions that read information for each construct across studies 


source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/ConvertEffectsizes.R')

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/PooledN.R') 

#source('/Users/aliya/my_docs/THESIS/BayesianMetaAnalysis/N_success.R')

#ai = N_success(N = data$N, likelihood_data$OR)


#data = read.csv('/Users/aliya/my_docs/THESIS/BayesianMetaAnalysis/Likelihood_data_clean.csv')

metaDataLikelihood = function(likelihood_data, Construct) {
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
  
  summary(Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset)
  
  LOGOdds_Ratio_result = predict(Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset)

  LOGOdds_Ratio = LOGOdds_Ratio_result$pred
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
                       LowerCI_LogOddsRatio = LowerCI_LogOddsRatio, 
                       UpperCI_LogOddsRatio = UpperCI_LogOddsRatio))
}

