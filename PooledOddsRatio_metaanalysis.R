
library(dplyr)
library(metafor)

x = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/input.csv')  # requires this data for all indexed functions that read information for each construct across studies 
print(x)


source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/ConvertEffectsizes.R')
print(likelihood_data)

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/PooledN.R') 

#source('/Users/aliya/my_docs/THESIS/BayesianMetaAnalysis/N_success.R')

#ai = N_success(N = data$N, likelihood_data$OR)


#data = read.csv('/Users/aliya/my_docs/THESIS/BayesianMetaAnalysis/Likelihood_data_clean.csv')

metaDataLikelihood = function(likelihood_data, Construct) {
  print("start calculating LOg Odds ratio")
  index = likelihood_data$Construct == Construct
  print(likelihood_data)
  likelihood_data_grouped = filter(likelihood_data, Construct == likelihood_data[index,]$Construct)
  print("we are in!")
  #Grouped_byConstruct_likelihoodData = filter(likelihood_data,likelihood_data$Construct == Construct)
  #print(Grouped_byConstruct_likelihoodData)
  #print("For this construct") 
  
  #print(Construct)

  #Non_pooled_lOR = Grouped_byConstruct_likelihoodData$lOR 
  
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
  print(LOGOdds_Ratio_result)
  
  print("Log Odds Ratio for this Construct is")
  print(Construct)
  LOGOdds_Ratio = LOGOdds_Ratio_result$pred
  LowerCI_LogOddsRatio = LOGOdds_Ratio_result$ci.lb
  UpperCI_LogOddsRatio = LOGOdds_Ratio_result$ci.ub
  print(LOGOdds_Ratio)
  
  
  k = Pooled_lOR_RandomEffect_from_ConverEffectSizes_Subset$k
  print("Pooled_OR_RandomEffect_from_ConverEffectSizes_Subset$k")
  print("For this construct")
  print(k)

  for (k in k){
    if(k == 1){LOGOdds_Ratio = likelihood_data_grouped$lOR
    }
    
  }
  
  # average Log Odds Ratio  
  
  print("Pooled Log Odds Ratio and other parameters, like variance and CI")
  print("For this construct")
  
  print(Construct)
  print("end calculating log odds ratio")
  
  #the actual Pooled Odds ration I need to pass on to other functions 
  
  print("the Pooled Odds Ratio for construct:")
  print(Construct)

  return(params = list(Construct = Construct, 
                       LOGOdds_Ratio = LOGOdds_Ratio, 
                       k = k, 
                       LowerCI_LogOddsRatio = LowerCI_LogOddsRatio, 
                       UpperCI_LogOddsRatio = UpperCI_LogOddsRatio))
}

