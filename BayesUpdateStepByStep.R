
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


BayesUpdateStepByStep = function(x, Construct) {
  
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
  
  BayesUpdate_func = function(Construct, Total_N_hyperprior, Mean_probability_hyperprior, Variance_hyperprior, Log_Odds_hyperprior, 
                              PriorExpert_N_PA_X, PriorExpert_N_noPA_noX, PriorExpert_N_noPA_X, PriorExpert_N_PA_noX, variance_expert_elicitation_task,
                              LOGOdds_Ratio_quant, variance_quant) {
    
    Probability = seq( 0 , 1 , length=1000)
    logOddsRatio = seq( -1 , 2 , length=1000)
    
    #elicit HYPERPRIOR distribution as a Gaussian (aka normal) distribution with mean and variance   from Jaarsma study. 
    
    
    #elicits hyperprior from arguments Total_N_hyperprior, Mean_probability_hyperprior, Variance_hyperprior, Probability
    Hyperprior_density = dnorm(Probability, Mean_probability_hyperprior,  Variance_hyperprior, log = FALSE)
    #normalised hyperprior distribution 
    Hyperprior_density = Hyperprior_density/sum(Hyperprior_density)
    data=data.frame(Probability,logOddsRatio,  Hyperprior_density, Mean_probability_hyperprior, Variance_hyperprior)
    data$Hyperprior_density_cumsum=cumsum(data$Hyperprior_density)
    data$Hyperprior_density_CI=ifelse(data$Hyperprior_density_cumsum<0.025|data$Hyperprior_density_cumsum>0.975, "outside CI", "inside CI")

    
    data$Log_Odds_hyperprior = Log_Odds_hyperprior
    data$Hyperprior_logOdds = dnorm(logOddsRatio, data$Log_Odds_hyperprior,  data$Variance_hyperprior, log = FALSE)
    data$Hyperprior_logOdds = data$Hyperprior_logOdds/sum(data$Hyperprior_logOdds)
    data$Hyperprior_logOdds_cumsum = cumsum(data$Hyperprior_logOdds)
    data$Hyperprior_logOdds_CI = ifelse(data$Hyperprior_logOdds_cumsum<0.025|data$Hyperprior_logOdds_cumsum>0.975, "outside CI", "inside CI")
    
    
    #elicit PRIOR 
    #On the basis of the results of the prior elicitation task we calculate the log OR for each construct
    data$logOR_expert_elicitation_task = log(PriorExpert_N_PA_X*PriorExpert_N_noPA_noX)/(PriorExpert_N_noPA_X*PriorExpert_N_PA_noX)
    #the density distribution for probability for physical activity given a construct according to the experts is centred around the logOR elicited from expert responses 
    data$variance_expert_elicitation_task = variance_expert_elicitation_task
    #prior distribution 
    data$Prior_qual_density = dnorm(logOddsRatio, data$logOR_expert_elicitation_task,  data$variance_expert_elicitation_task, log = FALSE)
    #normalise prior density distribution
    data$Prior_qual_density = data$Prior_qual_density/sum(data$Prior_qual_density)
    data$Prior_qual_density_cumsum=cumsum(data$Prior_qual_density)
    data$Prior_qual_density_CI=ifelse(data$Prior_qual_density_cumsum<0.025|data$Prior_qual_density_cumsum>0.975, "outside CI", "inside CI")
    data$p_Prior_qual = pnorm(logOddsRatio, data$logOR_expert_elicitation_task, data$variance_expert_elicitation_task, lower.tail = TRUE, log.p = FALSE)
    #Credible Intervals: prior distribution 
    data$Prior_qual_quantile_0.05 = qnorm(0.05, data$logOR_expert_elicitation_task, data$variance_expert_elicitation_task, lower.tail = TRUE, log.p = FALSE)
    data$Prior_qual_quantile_0.95 = qnorm(0.95,  data$logOR_expert_elicitation_task, data$variance_expert_elicitation_task, lower.tail = TRUE, log.p = FALSE)
    
    
    #to update the hyperprior with the qualitative results we use formula from Spiegelhalter et al., p 63: 
    data$Posterior_qual_only_mean = (data$Log_Odds_hyperprior/data$Variance_hyperprior + data$logOR_expert_elicitation_task/data$variance_expert_elicitation_task)/(1/data$Variance_hyperprior + 1/data$variance_expert_elicitation_task)
    data$Posterior_qual_only_variance = 1/(1/data$Variance_hyperprior +1/data$variance_expert_elicitation_task)
    #posterior distribution for updating hyperprior with prior 
    data$Posterior_qual_only = dnorm(logOddsRatio, data$Posterior_qual_only_mean, data$Posterior_qual_only_variance, log = FALSE)
    data$Posterior_qual_only = data$Posterior_qual_only/sum(data$Posterior_qual_only)
    data$Posterior_qual_only_cumsum = cumsum(data$Posterior_qual_only)
    data$Posterior_qual_only_CI=ifelse(data$Posterior_qual_only_cumsum<0.025|data$Posterior_qual_only_cumsum>0.975, "outside CI", "inside CI")
    #Credible Intervals: posterior for updating hyperprior with prior 
    data$Posterior_qual_only_quantile_0.05 = qnorm(0.05, data$Posterior_qual_only_mean, data$Posterior_qual_only_variance, lower.tail = TRUE, log.p = FALSE)
    data$Posterior_qual_only_quantile_0.95 = qnorm(0.95,  data$Posterior_qual_only_mean, data$Posterior_qual_only_variance, lower.tail = TRUE, log.p = FALSE)
    
    
    #Likelihood 
    data$LOGOdds_Ratio_quant = LOGOdds_Ratio_quant
    data$variance_quant = variance_quant
    # Likelihood distribution 
    data$Likelihood = dnorm(logOddsRatio, data$LOGOdds_Ratio_quant,  data$variance_quant, log = FALSE)
    data$Likelihood = data$Likelihood/sum(data$Likelihood)
    data$Likelihood_cumsum=cumsum(data$Likelihood)
    data$Likelihood_CI=ifelse(data$Likelihood_cumsum<0.025|data$Likelihood_cumsum>0.975, "outside CI", "inside CI")
    data$p_Likelihood = pnorm(logOddsRatio, data$LOGOdds_Ratio_quant, data$variance_quant, lower.tail = TRUE, log.p = FALSE)
    #Credible Intervals: Likelihood
    data$Likelihood_qual_quantile_0.05 = qnorm(0.05, data$LOGOdds_Ratio_quant, data$variance_quant, lower.tail = TRUE, log.p = FALSE)
    data$Likelihood_qual_quantile_0.95 = qnorm(0.95,  data$LOGOdds_Ratio_quant, data$variance_quant, lower.tail = TRUE, log.p = FALSE)
    

  
  
    #POSTERIOR
    #Formula from Spiegelhalter p 63: updating prior with likelihood using the following mean and variance for the distribution: 
    data$posterior_QualplusQuant_mean = (data$logOR_expert_elicitation_task/data$variance_expert_elicitation_task + data$LOGOdds_Ratio_quant/data$variance_quant)/(1/data$variance_expert_elicitation_task+1/data$variance_quant)
    data$posterior_QualplusQuant_variance =1/(1/data$variance_expert_elicitation_task+1/data$variance_quant)
    #posterior distribution for updating prior with likelihood  
    data$Posterior_QualplusQuant = dnorm(logOddsRatio, data$posterior_QualplusQuant_mean,  data$posterior_QualplusQuant_variance, log = FALSE)
    #normalise posterior density distribution
    data$Posterior_QualplusQuant = data$Posterior_QualplusQuant/sum(data$Posterior_QualplusQuant)
    data$Posterior_QualplusQuant_cumsum=cumsum(data$Posterior_QualplusQuant)
    data$Posterior_QualplusQuant_CI=ifelse(data$Posterior_QualplusQuant_cumsum<0.025|data$Posterior_QualplusQuant_cumsum>0.975, "outside CI", "inside CI")
    data$p_Posterior_QualplusQuant = pnorm(logOddsRatio, data$posterior_QualplusQuant_mean, data$posterior_QualplusQuant_variance, lower.tail = TRUE, log.p = FALSE)
    #Credible Intervals: posterior (update without hyperprior)
    data$Posterior_QualplusQuant_quantile_0.05 = qnorm(0.05, data$posterior_QualplusQuant_mean, data$posterior_QualplusQuant_variance, lower.tail = TRUE, log.p = FALSE)
    data$Posterior_QualplusQuant_quantile_0.95 = qnorm(0.95,  data$posterior_QualplusQuant_mean, data$posterior_QualplusQuant_variance, lower.tail = TRUE, log.p = FALSE)
    
    #Posterior full analysis (including the hyperprior)
    data$posterior_All_mean = (data$Posterior_qual_only_mean/data$Posterior_qual_only_variance + data$LOGOdds_Ratio_quant/data$variance_quant)/(1/data$Posterior_qual_only_variance+1/data$variance_quant)
    data$posterior_All_variance =1/(1/data$Posterior_qual_only_variance+1/data$variance_quant)
    data$posterior_All = dnorm(logOddsRatio, data$posterior_All_mean, data$posterior_All_variance, log = FALSE)
    data$posterior_All = data$posterior_All/sum(data$posterior_All)
    data$posterior_All_cumsum = cumsum(data$posterior_All)
    data$posterior_All_CI = ifelse(data$posterior_All_cumsum<0.025|data$posterior_All_cumsum>0.975, "outside CI", "inside CI")
    data$posterior_All_quantile_0.05 = qnorm(0.05, data$posterior_All_mean, data$posterior_All_variance, lower.tail = TRUE, log.p = FALSE)
    data$posterior_All_quantile_0.95 = qnorm(0.95,  data$posterior_All_mean, data$posterior_All_variance, lower.tail = TRUE, log.p = FALSE)
    
    data
  }
  
  data=BayesUpdate_func(Construct = Construct, 
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
  
  
    
  #function for ploting densities, colour values: - grey: "#999999", lilac: "#CC79A7", blue: "#0072B2". These are suitable for colour blind people)
  plotting=function(data, ... ,title, values_colour ) {
    library(ggplot2)
    ggplot(data, ...)+
      geom_bar(stat="identity", alpha=0.5)+
      theme_minimal()+
      theme(axis.text.y = element_blank())+
      ylab("Probability density")+
      ggtitle(title)+
      labs(fill='95% confidence interval')+
      scale_fill_manual(values = values_colour)
    
  }
  
  #plot hyperprior density distribution 
  plot_hyperprior_density = plotting(data=data,
                                     aes(x=Probability, y=Hyperprior_density, fill=Hyperprior_density_CI), 
                                     values_colour = c("#999999", "#0072B2"), 
                                     title="Hyperprior: Probability")
  
  
  ggsave(file = paste(OUTPUT_ROOT, "/plot_hyperprior_density.pdf",  sep=""),plot_hyperprior_density, width=4, height=3, units="in", scale=3)
  
  
  #print plot, so  it can be saved into the local repository 
  print(plot_hyperprior_density)

  plot_hyperprior_density_Log_OR = plotting(data=data,
                                     aes(x=logOddsRatio, y=Hyperprior_logOdds, fill=Hyperprior_logOdds_CI), 
                                     values_colour = c("#999999", "#0072B2"), 
                                     title="Hyperprior: log Odds Ratio")
  
  print(plot_hyperprior_density_Log_OR)


  # plot prior 
  plot_Prior_Qual_density = plotting(data=data,
                                     aes(x=logOddsRatio, y=Prior_qual_density, fill= Prior_qual_density_CI), 
                                     values_colour = c("#CC79A7", "#0072B2"), 
                                     title = paste("Expert Belief: Probability for physical activity given", print(Construct)))
  
  print(plot_Prior_Qual_density)
  
  
  #plot posterior_qual only 
  plot_Posterior_qual_only = plotting(data=data,
                                        aes(x=logOddsRatio, y= Posterior_qual_only, fill= Posterior_qual_only_CI), 
                                        values_colour = c("#CC79A7", "#0072B2"), 
                                        title = paste("Posterior distribution for physical activity according to qualitative evidence (with hyperprior):", print(Construct)))
  
  print(plot_Posterior_qual_only)
  
  
  #plot likelihood 
  plot_Likelihood_density = plotting(data=data,
                                     aes(x=logOddsRatio, y=Likelihood, fill= Likelihood_CI), 
                                     values_colour = c("#009E73", "#0072B2"), 
                                     title = paste("Likelihood: Probability for physical activity given", print(Construct)))
  
  print(plot_Likelihood_density)
  


  #update the qualitative evidence with quantitative evidence
  
  plot_posterior_QualplusQuant_density = plotting(data=data,
                                          aes(x=logOddsRatio, y= Posterior_QualplusQuant, fill= Posterior_QualplusQuant_CI), 
                                          values_colour = c("#D55E00", "#0072B2"), 
                                          title = paste("Posterior distribution for physical activity according to qualitative and quantitative evidence:", print(Construct)))
  
  
  print(plot_posterior_QualplusQuant_density)


  
  
  return(params = (data.frame(Construct = Construct, 
                              data)))
                             
}


