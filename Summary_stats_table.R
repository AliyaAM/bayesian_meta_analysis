



source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep="")) #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 

source(paste(SOURCE_ROOT, "PooledN.R", sep=""))  # calculate the total number of participants (N) across studies that evaluated each construct, read from the csv file QuantData

source(paste(SOURCE_ROOT, "PooledOddsRatio_metaanalysis.R", sep="")) #run the frequentisit meta-analysis (REML) pooling the findings from quantitative studies, stratified by construct. The Overall Effect estimate is (log) Odds Ratio, a list of pooled Log ORs, one per construct). 


# this function below produces  density, distribution and all summary statistic for: 
#############################################################  (a) the prior: the levels of physical activity worldwide (Jaarsma et al., 2013); 
#############################################################  (b) the likelihood (pooled estimates from teh quantitative studies);
#############################################################  (c) and posterior (the bayes update implimmmented per Spighelhalter et al., 2004 reccomendation).
#function for computin prior, likelihood and posterior density. reference: https://rpubs.com/RRisto/betadist


Summary_stats_table <- function(data, Construct) {
  
  index = x$Construct == Construct
  #data for the HYPERPRIOR: 
  JaarsmaInternationalStudy = JaarsmaInternationalStudy
  #Total N, variance and mean estimate for the probability of physical activity in general HF populationfrom Jaarsma study (empirical hyperprior):    
  Total_N_hyperprior = JaarsmaInternationalStudy$TotalN[20]
  Variance_hyperprior = JaarsmaInternationalStudy$Variance[20]
  Mean_probability_hyperprior = JaarsmaInternationalStudy$Proportion_highPA[20]
  Log_Odds_hyperprior = log(JaarsmaInternationalStudy$N_highPA[20]/JaarsmaInternationalStudy$N_lowPA[20])
  
  
  #data for the LIKELIHOOD 
  #calculate the total N across quant studies, stratified by construct: 
  PooledN_output = PooledN(data = data, Construct = Construct)
  N = PooledN_output$N
  #run the frequentisit meta-analysis (REML) pooling the findings from quantitative studies, stratified by construct. The Overall Effect estimate is (log) Odds Ratio, a list of pooled Log ORs, one per construct). 
  #using function: PooledOddsRatio_metaanalysis.R, which runs metafor library 
  meta_data_likelihoodResults = metaDataLikelihood(likelihood_data = likelihood_data, Construct = Construct, N = N)
  LOGOdds_Ratio_quant = meta_data_likelihoodResults$LOGOdds_Ratio
  Standard_deviation_quant = meta_data_likelihoodResults$Standard_deviation_LogOR
  
  #variance_quant = Standard_deviation_quant^2
  
  variance_quant = meta_data_likelihoodResults$variance_Pooled_lOR 
  
  LowerCI_LogOddsRatio = meta_data_likelihoodResults$LowerCI_LogOddsRatio 
  UpperCI_LogOddsRatio = meta_data_likelihoodResults$UpperCI_LogOddsRatio
  #the number of quantitative studies that evaluted each construct: 
  k =  meta_data_likelihoodResults$k


Summary_statistics_table = function(Construct, Total_N_hyperprior, Mean_probability_hyperprior, Variance_hyperprior, Log_Odds_hyperprior, 
                                    LOGOdds_Ratio_quant, variance_quant) {
  
  summary_data = data.frame(Variance_hyperprior, Log_Odds_hyperprior)
  
  Hyperprior_quantile_0.50 = qnorm(0.50, Log_Odds_hyperprior, Variance_hyperprior, lower.tail = TRUE, log.p = FALSE)
  summary_data = cbind(summary_data, Hyperprior_quantile_0.50)
  
  Hyperprior_quantile_0.05 = qnorm(0.05, Log_Odds_hyperprior, Variance_hyperprior, lower.tail = TRUE, log.p = FALSE)
  Hyperprior_quantile_0.95 = qnorm(0.95,  Log_Odds_hyperprior, Variance_hyperprior, lower.tail = TRUE, log.p = FALSE)
  
  
  summary_data = cbind(summary_data, Hyperprior_quantile_0.05)
  summary_data = cbind(summary_data, Hyperprior_quantile_0.95)
  LOGOdds_Ratio_quant = LOGOdds_Ratio_quant
  summary_data = cbind(summary_data, LOGOdds_Ratio_quant)
  
  variance_quant = variance_quant
  summary_data = cbind(summary_data, variance_quant)
  Likelihood_qual_quantile_0.50 = qnorm(0.50, LOGOdds_Ratio_quant, variance_quant, lower.tail = TRUE, log.p = FALSE)
  Likelihood_qual_quantile_0.05 = qnorm(0.05, LOGOdds_Ratio_quant, variance_quant, lower.tail = TRUE, log.p = FALSE)
  Likelihood_qual_quantile_0.95 = qnorm(0.95,  LOGOdds_Ratio_quant, variance_quant, lower.tail = TRUE, log.p = FALSE)
  
  summary_data = cbind(summary_data, Likelihood_qual_quantile_0.50)
  summary_data = cbind(summary_data, Likelihood_qual_quantile_0.05)
  summary_data = cbind(summary_data, Likelihood_qual_quantile_0.95)
  
  return(summary_data)
}

summary_data = Summary_statistics_table(Construct = Construct, 
                                        Total_N_hyperprior = Total_N_hyperprior, 
                                        Mean_probability_hyperprior = Mean_probability_hyperprior, 
                                        Variance_hyperprior = Variance_hyperprior,
                                        Log_Odds_hyperprior = Log_Odds_hyperprior, 
                                        LOGOdds_Ratio_quant = LOGOdds_Ratio_quant, 
                                        variance_quant = variance_quant)


return(params = (data.frame(Construct = Construct, 
                            summary_data)))


}
