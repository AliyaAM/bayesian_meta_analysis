
#this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 

library(ggplot2)
library(reshape2) 
library(dplyr)
library(tibble)



source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep=""))   #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be compatible with qualitative evidence, we treated all results as binary. 

source(paste(SOURCE_ROOT, "PooledN.R", sep="")) # calculate the total number of participants (N) across studies that evaluated each construct, read from the csv file QuantData

source(paste(SOURCE_ROOT, "PooledOddsRatio_metaanalysis.R", sep=""))  #run the frequentist meta-analysis (REML) pooling the findings from quantitative studies, stratified by construct. The Overall Effect estimate is (log) Odds Ratio, a list of pooled Log ORs, one per construct). 



#function for computing prior, likelihood and posterior density. reference: https://rpubs.com/RRisto/betadist

#  below we will  produce distribution density for the: 
###### (a) HYPERPRIOR: the levels of physical activity in HF worldwide, general physical activity levels in HF estimated from a large international study (Jaarsma et al., 2013),
###### (b) the Prior elicited from the expert elicitation task which was based on the qualitative evidence; 
###### (c) Likelihood (pooled log OR estimates from the quantitative studies); 
###### (d) and Posterior (the posterior distribution obtained by means of Bayes update (first update Hyperprior with Prior, the update that with the Likelihood). Bayes Update is implemented per Spiegelhalter et al., 2013 recommendations) 
#reference: Spiegelhalter DJ, Abrams KR, Myles JP. Bayesian Approaches to Clinical Trials and Health-Care Evaluation. Chichester, UK: John Wiley & Sons, Ltd; 2003


Summary_stats_table_qual_and_quant = function(x, Construct) {
  
  #below we index the data by the name of construct
  index = x$Construct == Construct
  
  #data for the HYPERPRIOR: 
  JaarsmaInternationalStudy = JaarsmaInternationalStudy
  #Total N, variance and mean estimate for the probability of physical activity in general HF population from Jaarsma study (empirical hyperprior):    
  Total_N_hyperprior = JaarsmaInternationalStudy$TotalN[20]
  Variance_hyperprior = JaarsmaInternationalStudy$Variance[20]
  Mean_probability_hyperprior = JaarsmaInternationalStudy$Proportion_highPA[20]
  
  
  
  Log_Odds_hyperprior = log(JaarsmaInternationalStudy$N_highPA[20]/JaarsmaInternationalStudy$N_lowPA[20])
  
  #data for PRIOR 
  #Six experts completed the expert elicitation task. 
  #The reviewers made a judgement on whether the hypothetical HF patient met the recommended levels of physical activity or not. 
  #The number of scenarios where the construct was present (ie., X = 1) and the experts judged them as being likely to be physically active based on the qualitative studies they read (PA = 1) was expressed as N when PA = 1 and X = 1 (ie., physically active given the construct). 
  #Likewise, we calculated the number of physically active given the construct is absent (X = 0), ie  N – N_(PA=1;X=1 ). 
  # It is important to note that we used the entire set of data, meaning that the total pool of HF patients would equal to 30 scenarios multiplied by six different expert judgements (30 x 6).  
  
  PriorExpert_N_PA_X = x[index,]$PriorExpert_N_PA_X
  PriorExpert_N_noPA_noX = x[index,]$PriorExpert_N_noPA_noX
  PriorExpert_N_noPA_X = x[index,]$PriorExpert_N_noPA_X
  PriorExpert_N_PA_noX = x[index,]$PriorExpert_N_PA_noX
  variance_expert_elicitation_task = x[index,]$variance
  
  #data for the LIKELIHOOD 
  #calculate the total N across quant studies, stratified by construct: 
  PooledN_output = PooledN(data = data, Construct = Construct)
  N = PooledN_output$N
  #run the frequentist  meta-analysis (REML) pooling the findings from quantitative studies, stratified by construct. The Overall Effect estimate is (log) Odds Ratio, a list of pooled Log ORs, one per construct). 
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
  
  Summary_statistics_table_qual_quant = function(Construct, Total_N_hyperprior, Mean_probability_hyperprior, Variance_hyperprior, Log_Odds_hyperprior, 
                                      PriorExpert_N_PA_X, PriorExpert_N_noPA_noX, PriorExpert_N_noPA_X, PriorExpert_N_PA_noX, variance_expert_elicitation_task,
                                      LOGOdds_Ratio_quant, variance_quant) {
    
    Variance_hyperprior = Variance_hyperprior 
    Log_Odds_hyperprior = Log_Odds_hyperprior
    
    summary_data = cbind(Variance_hyperprior, Log_Odds_hyperprior)
    
    Hyperprior_quantile_0.50 = qnorm(0.50, Log_Odds_hyperprior, Variance_hyperprior, lower.tail = TRUE, log.p = FALSE)
    summary_data = cbind(summary_data, Hyperprior_quantile_0.50)
    
    Hyperprior_quantile_0.05 = qnorm(0.05, Log_Odds_hyperprior, Variance_hyperprior, lower.tail = TRUE, log.p = FALSE)
    Hyperprior_quantile_0.95 = qnorm(0.95,  Log_Odds_hyperprior, Variance_hyperprior, lower.tail = TRUE, log.p = FALSE)
    
    
    summary_data = cbind(summary_data, Hyperprior_quantile_0.05)
    summary_data = cbind(summary_data, Hyperprior_quantile_0.95)
    

    #On the basis of the results of the prior elicitation task we calculate the log OR for each construct
    logOR_expert_elicitation_task = log(PriorExpert_N_PA_X*PriorExpert_N_noPA_noX)/(PriorExpert_N_noPA_X*PriorExpert_N_PA_noX)
    
    summary_data = cbind(summary_data, logOR_expert_elicitation_task)
    
    
    #the density distribution for probability for physical activity given a construct according to the experts is centred around the logOR elicited from expert responses 
    variance_expert_elicitation_task = variance_expert_elicitation_task
    summary_data = cbind(summary_data, variance_expert_elicitation_task)
    
    Prior_qual_quantile_0.50 = qnorm(0.50, logOR_expert_elicitation_task, variance_expert_elicitation_task, lower.tail = TRUE, log.p = FALSE)
    summary_data = cbind(summary_data, Prior_qual_quantile_0.50)
    
    #Credible Intervals: prior distribution 
    Prior_qual_quantile_0.05 = qnorm(0.05, logOR_expert_elicitation_task, variance_expert_elicitation_task, lower.tail = TRUE, log.p = FALSE)
    Prior_qual_quantile_0.95 = qnorm(0.95,  logOR_expert_elicitation_task, variance_expert_elicitation_task, lower.tail = TRUE, log.p = FALSE)
    
    summary_data = cbind(summary_data, Prior_qual_quantile_0.05)
    summary_data = cbind(summary_data, Prior_qual_quantile_0.95)
    
    
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
    
    #POSTERIOR
    #Formula from Spiegelhalter p 63: updating prior with likelihood using the following mean and variance for the distribution: 
    posterior_QualplusQuant_mean = (logOR_expert_elicitation_task/variance_expert_elicitation_task + LOGOdds_Ratio_quant/variance_quant)/(1/variance_expert_elicitation_task+1/variance_quant)
    posterior_QualplusQuant_variance =1/(1/variance_expert_elicitation_task+1/variance_quant)
    summary_data = cbind(summary_data, posterior_QualplusQuant_mean)
    summary_data = cbind(summary_data, posterior_QualplusQuant_variance)
    
    Posterior_QualplusQuant_quantile_0.50 = qnorm(0.50,posterior_QualplusQuant_mean, posterior_QualplusQuant_variance, lower.tail = TRUE, log.p = FALSE)
    summary_data = cbind(summary_data, Posterior_QualplusQuant_quantile_0.50)
    
    #Credible Intervals: posterior (update without hyperprior)
    Posterior_QualplusQuant_quantile_0.05 = qnorm(0.05,posterior_QualplusQuant_mean, posterior_QualplusQuant_variance, lower.tail = TRUE, log.p = FALSE)
    Posterior_QualplusQuant_quantile_0.95 = qnorm(0.95,  posterior_QualplusQuant_mean, posterior_QualplusQuant_variance, lower.tail = TRUE, log.p = FALSE)
    summary_data = cbind(summary_data, Posterior_QualplusQuant_quantile_0.05)
    summary_data = cbind(summary_data, Posterior_QualplusQuant_quantile_0.95)
    
    return(summary_data)
  }
  
  summary_data = Summary_statistics_table_qual_quant(Construct = Construct, 
                                          Total_N_hyperprior = Total_N_hyperprior, 
                                          Mean_probability_hyperprior = Mean_probability_hyperprior, 
                                          Variance_hyperprior = Variance_hyperprior,
                                          Log_Odds_hyperprior = Log_Odds_hyperprior, 
                                          PriorExpert_N_PA_X = PriorExpert_N_PA_X, 
                                          PriorExpert_N_noPA_noX = PriorExpert_N_noPA_noX, 
                                          PriorExpert_N_noPA_X = PriorExpert_N_noPA_X,
                                          PriorExpert_N_PA_noX = PriorExpert_N_PA_noX, 
                                          variance_expert_elicitation_task = variance_expert_elicitation_task,
                                          LOGOdds_Ratio_quant = LOGOdds_Ratio_quant, 
                                          variance_quant = variance_quant)
  
  
  return(params = (data.frame(Construct = Construct, 
                              summary_data)))
  
  
}


  