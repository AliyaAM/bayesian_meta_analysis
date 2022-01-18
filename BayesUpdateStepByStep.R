
#this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 

library(ggplot2)
library(reshape2) 
library(dplyr)
library(tibble)


x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from from the quantitative studies 
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)

source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep=""))   #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 

source(paste(SOURCE_ROOT, "PooledN.R", sep="")) # calculate the total number of participants (N) across studies that evaluated each construct, read from the csv file QuantData

source(paste(SOURCE_ROOT, "PooledOddsRatio_metaanalysis.R", sep=""))  #run the frequentisit meta-analysis (REML) pooling the findings from quantitative studies, stratified by construct. The Overall Effect estimate is (log) Odds Ratio, a list of pooled Log ORs, one per construct). 

#source(paste(SOURCE_ROOT, "ContingenciesTable_MCMC.R", sep="")) #estimate the contingencies table for each study using Monte Carlo Markov Chain rejection sampling (the total N and Log odds ratio formulas are used as the rejection criteria)

source(paste(SOURCE_ROOT, "Density_ggplot.R", sep="")) #produce illustrations: plot density and distribution of probabality of physical activity happening given the construct. 

source(paste(SOURCE_ROOT, "N_success.R", sep=""))  #calculate the proportion of total sample engaging in physical activity when construct is present



#function for computin prior, likelihood and posterior density. reference: https://rpubs.com/RRisto/betadist
#function for metropolis-hestings sampling, reference: https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/


#  below we will  produce distribution density for the: 
###### (a) HYPERPRIOR: the levels of physical activity in HF worldwide, general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013),
###### (b) the Prior elicited from the expert elicitation task which was based on the qualitative evidence; 
###### (c) Likelihood (pooled log OR estimates from the quantitative studies); 
###### (d) and Posterior (the posterior distribution obtained by means of Bayes update (first update Hyperprior with Prior, the update that with the Likelihood). Bayes Update is implimmmented per Spighelhalter et al., 2013 reccomendations) 
#reference: Spiegelhalter DJ, Abrams KR, Myles JP. Bayesian Approaches to Clinical Trials and Health-Care Evaluation. Chichester, UK: John Wiley & Sons, Ltd; 2003


BayesUpdateStepByStep = function(x, Construct, uncertainty, seed) {
  
  #below we index the data by the name of construct
  index = x$Construct == Construct
  
  #data for the HYPERPRIOR: 
  JaarsmaInternationalStudy = JaarsmaInternationalStudy
  
  #Total N, variance and mean estimate for the probability of physical activity in general HF populationfrom Jaarsma study (empirical hyperprior):    
  Total_N_hyperprior = JaarsmaInternationalStudy$TotalN[20]
  Variance_hyperprior = JaarsmaInternationalStudy$Variance[20]
  Mean_probability_hyperprior = JaarsmaInternationalStudy$Proportion_highPA[20]
  
  #elicit HYPERPRIOR distribution as a Gaussian (aka normal) distribution with mean value = mean from Jaarsma, and variance from Jaarsma 
  
  
    ### Hyperprior_func elicits hyperprior from arguments Total_N_hyperprior, Mean_probability_hyperprior, Variance_hyperprior, Probability
  Hyperprior_func=function(Total_N_hyperprior, Mean_probability_hyperprior, Variance_hyperprior) {
    
    Probability = seq( 0 , 1 , length=1000)
    logOddsRatio = seq( -1 , 1 , length=1000)
    
    Hyperprior_density = dnorm(Probability, Mean_probability_hyperprior,  Variance_hyperprior, log = FALSE)
    Hyperprior_density = Hyperprior_density/sum(Hyperprior_density)
    
    
    data=data.frame(Probability, Hyperprior_density)
    data$Hyperprior_density_cumsum=cumsum(data$Hyperprior_density)
    data$Hyperprior_density_CI=ifelse(data$Hyperprior_density_cumsum<0.025|data$Hyperprior_density_cumsum>0.975, "outside CI", "inside CI")
    
    data
  }
  
  #function for ploting densities, colour values: - grey: "#999999", lilac: "#CC79A7", blue: "#0072B2". This are suitable for colour blind people)
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
  #elicit hyperprior  
  data_density_Hyperprior=Hyperprior_func(Total_N_hyperprior = Total_N_hyperprior, Mean_probability_hyperprior = Mean_probability_hyperprior, Variance_hyperprior = Variance_hyperprior)
 
  #plot hyperprior density distribution 
  plot_hyperprior_density = plotting(data=data_density_Hyperprior,
                                     aes(x=Probability, y=Hyperprior_density, fill=Hyperprior_density_CI), 
                                     values_colour = c("#999999", "#0072B2"), 
                                     title="Hyperprior")
  
  
  #print plot, so we it can be saved into the local repository 
  print(plot_hyperprior_density)

  #Six experts completed the expert elicitation task. 
  #The reviewers made a judgement on whether the hypothetical HF patient met the recommended levels of physical activity or not. 
  #The number of scenarios where the construct was present (ie., X = 1) and the experts judged them as being likely to be physically active based on the qualitative studies they read (PA = 1) was expressed as N when PA = 1 and X = 1 (ie., physically active given the construct). 
  #Likewise, we calculated the number of physically active given the construct is absent (X = 0), ie  N – N_(PA=1;X=1 ). 
  # It is important to note that we used the entire set of data, meaning that the total pool of HF patients would equal to 30 scenarios multiplied by six different expert judgements (30 x 6).  
  #From this, we estimated alpha and beta parameters for the likelihood distribution using the following formulas (from Spiegelhalter et al 2003 (3): α+N_(PA=1;X=1 ); ß + N – N_█(PA=1;X=1 @).
  #Using these parameters, we elicited likelihood distribution using dbeta function in R. 
  

  Age_N_PA_X =28
  SelfEfficacy_N_PA_X = 41
  SocialSupport_N_PA_X = 38
  Comorbidity_N_PA_X = 23 
  NegativeAttitute_N_PA_X = 4
  Soma_N_PA_X = 3
  Soma_N_PA_X = 3
  Symptoms_N_PA_X = 7
  Soma_N_PA_X = 3
  PositiveAttitute_N_PA_X = 4
  
  PriorExpert_N_PA_X = c(Age_N_PA_X,
                         SelfEfficacy_N_PA_X,
                         SocialSupport_N_PA_X,
                         Comorbidity_N_PA_X,
                         NegativeAttitute_N_PA_X,
                         Soma_N_PA_X,
                         Soma_N_PA_X,
                         Symptoms_N_PA_X,
                         Soma_N_PA_X,
                         PositiveAttitute_N_PA_X)
  
  
  Age_N_PA_noX =30
  SelfEfficacy_N_PA_noX = 17
  SocialSupport_N_PA_noX = 20
  Comorbidity_N_PA_noX = 35
  NegativeAttitute_N_PA_noX = 4
  Soma_N_PA_noX =10
  Soma_N_PA_noX =10
  Symptoms_N_PA_noX = 6
  Soma_N_PA_noX =10
  PositiveAttitute_N_PA_noX = 4
  
  
  PriorExpert_N_PA_noX = c(Age_N_PA_noX,
                           SelfEfficacy_N_PA_noX,
                           SocialSupport_N_PA_noX,
                           Comorbidity_N_PA_noX,
                           NegativeAttitute_N_PA_noX,
                           Soma_N_PA_noX,
                           Soma_N_PA_noX,
                           Symptoms_N_PA_noX,
                           Soma_N_PA_noX,
                           PositiveAttitute_N_PA_noX)
  
  
  Age_N_noPA_X = 20
  SelfEfficacy_N_noPA_X = 7
  SocialSupport_N_noPA_X = 10
  Comorbidity_N_noPA_X = 25 
  NegativeAttitute_N_noPA_X = 4
  Soma_N_noPA_X = 17 
  Soma_N_noPA_X = 17
  Symptoms_N_noPA_X = 13
  Soma_N_noPA_X = 17
  PositiveAttitute_N_noPA_X = 1
  
  PriorExpert_N_noPA_X = c(Age_N_noPA_X,
                           SelfEfficacy_N_noPA_X,
                           SocialSupport_N_noPA_X,
                           Comorbidity_N_noPA_X,
                           NegativeAttitute_N_noPA_X,
                           Soma_N_noPA_X,
                           Soma_N_noPA_X,
                           Symptoms_N_noPA_X,
                           Soma_N_noPA_X,
                           PositiveAttitute_N_noPA_X)
  
  
  Age_N_noPA_noX =18 
  SelfEfficacy_N_noPA_noX = 31
  SocialSupport_N_noPA_noX =28
  Comorbidity_N_noPA_noX = 13
  NegativeAttitute_N_noPA_noX = 1
  Soma_N_noPA_noX = 10
  Soma_N_noPA_noX = 10
  Symptoms_N_noPA_noX = 14
  Soma_N_noPA_noX = 10
  PositiveAttitute_N_noPA_noX = 4
  
  PriorExpert_N_noPA_noX = c(Age_N_noPA_noX,
                             SelfEfficacy_N_noPA_noX,
                             SocialSupport_N_noPA_noX,
                             Comorbidity_N_noPA_noX,
                             NegativeAttitute_N_noPA_noX,
                             Soma_N_noPA_noX,
                             Soma_N_noPA_noX,
                             Symptoms_N_noPA_noX,
                             Soma_N_noPA_noX,
                             PositiveAttitute_N_noPA_noX)
  
  Number_successes = cbind(PriorExpert_N_PA_X,
                           PriorExpert_N_PA_noX,
                           PriorExpert_N_noPA_X,
                           PriorExpert_N_noPA_noX) 
 
  x = cbind(x, Number_successes)

  #On the basis of the results of the prior elicitation task we calculate the log OR for each construct
  
  PriorExpert_N_PA_X = x[index,]$PriorExpert_N_PA_X
  PriorExpert_N_noPA_noX = x[index,]$PriorExpert_N_noPA_noX
  PriorExpert_N_noPA_X = x[index,]$PriorExpert_N_noPA_X
  PriorExpert_N_PA_noX = x[index,]$PriorExpert_N_PA_noX
  
  # PlogOR_expert_elicitation_task PA given Construct (i.e, Probability_PA_X, condiitonal probability) according to the expert elicitation task 
  
  logOR_expert_elicitation_task = log((PriorExpert_N_PA_X*PriorExpert_N_noPA_noX)/(PriorExpert_N_noPA_X*PriorExpert_N_PA_noX))

  #Uncertainty was elicited from teh variance in responses from different experts 
  variance_expert_elicitation_task = x[index,]$variance
  
  #the total n (scenarios*rater) is 150 (i.e., 150 scenario-rater pairs)
  Total_N_scenario_expert_pairs = 150

  #we elicit the probability distribution with these two parameters
  Prior_qual = rnorm(Total_N_scenario_expert_pairs, logOR_expert_elicitation_task, variance_expert_elicitation_task)
  
  #Prior_qual Credible Interval are calculated below:
  logOddsRatio = seq( -1 , 1 , length=1000)
  
  p_Prior_qual = pnorm(logOddsRatio, logOR_expert_elicitation_task, variance_expert_elicitation_task, lower.tail = TRUE, log.p = FALSE)
  Prior_qual_quantile_0.05 = qnorm(0.05, logOR_expert_elicitation_task, variance_expert_elicitation_task, lower.tail = TRUE, log.p = FALSE)
  Prior_qual_quantile_0.95 = qnorm(0.95,  logOR_expert_elicitation_task, variance_expert_elicitation_task, lower.tail = TRUE, log.p = FALSE)
  
####
  #function for distribution elicited from the experts 
  
  Prior_qual_func = function(logOR_expert_elicitation_task,  variance_expert_elicitation_task) {
    Probability = seq( 0 , 1 , length=1000)
    logOddsRatio = seq( -1 , 1 , length=1000)
    ProbabilityPrior_qual_func = seq( 0 , 1 , length=1000)
    
    
    print(logOR_expert_elicitation_task)
    print(variance_expert_elicitation_task)

    # The density distribution is centered on Probabity of PA given Construct (i.e, logOR_expert_elicitation_task, condiitonal probability) according to the expert elicitation task 

    Prior_qual_density = dnorm(ProbabilityPrior_qual_func, logOR_expert_elicitation_task,  variance_expert_elicitation_task, log = TRUE)
    #normalise density distribution
    Prior_qual_density = Prior_qual_density/sum(Prior_qual_density)
    
    
    data=data.frame(ProbabilityPrior_qual_func, Prior_qual_density)
    data$Prior_qual_density_cumsum=cumsum(data$Prior_qual_density)
    data$Prior_qual_density_CI=ifelse(data$Prior_qual_density_cumsum<0.025|data$Prior_qual_density_cumsum>0.975, "outside CI", "inside CI")
    
    data
  }
  

  #elicit density distribution from the expert elicitation task 
  data_density_Prior_qual = Prior_qual_func(logOR_expert_elicitation_task = logOR_expert_elicitation_task,  
                                            variance_expert_elicitation_task = variance_expert_elicitation_task)


  #below we are updating Hyperprior with Qualitative Results elicited from the expert elicitation task for each construct separately: 
  #formulas for calculating posterior distribution parameters below are from Spieghelhalter et al. 2003, p63, equation 3.15: 

  
  posterior_Qual_mean = (Mean_probability_hyperprior/Variance_hyperprior + logOR_expert_elicitation_task/variance_expert_elicitation_task)/(1/Variance_hyperprior+1/variance_expert_elicitation_task)
  posterior_Qual_variance =1/(1/Variance_hyperprior+1/variance_expert_elicitation_task)
  
  
  #below we elicit posterior_Qual which is distirbution for probability for physical activity given a construct accroding to the qualitative evidence
  posterior_Qual_func=function(posterior_Qual_mean, posterior_Qual_variance) {
  
      
    Probability = seq( 0 , 1 , length=1000)
    logOddsRatio = seq( -1 , 1 , length=1000)
    ProbabilityPrior_qual_func = seq( 0 , 1 , length=1000)
    
    #the density distribution for probability for phyical activity given a construct according to teh experts is centred around the logOR elicited from expert responses 
    posterior_Qual_density = dnorm(logOddsRatio, logOR_expert_elicitation_task,  variance_expert_elicitation_task, log = FALSE)
    posterior_Qual_density = posterior_Qual_density/sum(posterior_Qual_density)
    
    
    data=data.frame(logOddsRatio, posterior_Qual_density)
    data$posterior_Qual_density_cumsum=cumsum(data$posterior_Qual_density)
    data$posterior_Qual_density_CI=ifelse(data$posterior_Qual_density_cumsum<0.025|data$posterior_Qual_density_cumsum>0.975, "outside CI", "inside CI")
    
    #posterior_Qual Credible Interval are calculated below:
    data$p_posterior_Qual = pnorm(logOddsRatio, posterior_Qual_mean, posterior_Qual_variance, lower.tail = TRUE, log.p = FALSE)
    data$posterior_Qual_quantile_0.05 = qnorm(0.05, posterior_Qual_mean, posterior_Qual_variance, lower.tail = TRUE, log.p = FALSE)
    data$posterior_Qual_quantile_0.95 = qnorm(0.95,  posterior_Qual_mean, posterior_Qual_variance, lower.tail = TRUE, log.p = FALSE)
    
    data
  }
  
  
  data_posterior_Qual = posterior_Qual_func(posterior_Qual_mean, posterior_Qual_variance)
  

  
  plot_posterior_Qual_density = plotting(data=data_posterior_Qual,
                                     aes(x=logOddsRatio, y=posterior_Qual_density, fill=posterior_Qual_density_CI), 
                                     values_colour = c("#CC79A7", "#0072B2"), 
                                     title = paste("Expert Belief: Probability of physical activity given", print(Construct)))
  
  
  print(plot_posterior_Qual_density)
  
  #calculate the TOTAL N across quant studies, stratified by construct: 
  PooledN_output = PooledN(data = data, Construct = Construct)
  N = PooledN_output$N

  #run the frequentisit meta-analysis (REML) pooling the findings from quantitative studies, stratified by construct. The Overall Effect estimate is (log) Odds Ratio, a list of pooled Log ORs, one per construct). 
  #using function: PooledOddsRatio_metaanalysis.R, which runs metafor library 
  
  meta_data_likelihoodResults = metaDataLikelihood(likelihood_data = likelihood_data, Construct = Construct)
  LOGOdds_Ratio = meta_data_likelihoodResults$LOGOdds_Ratio
  
  
  #the number of quantitative studies that evaluted each construct: 
  k =  meta_data_likelihoodResults$k

  LowerCI_LogOddsRatio = meta_data_likelihoodResults$LowerCI_LogOddsRatio 
  UpperCI_LogOddsRatio = meta_data_likelihoodResults$UpperCI_LogOddsRatio
  
  
  
  
  return(params = (data.frame(Construct = Construct, 
                              #prior_alpha = HyperPrior_a, 
                              #prior_beta = HyperPrior_b, 
                              #PriorMean = PriorMean, 
                              #PriorMode = PriorMode,
                              #prior_CredibleInterval_0.05 = prior_quantile_0.05, 
                              #prior_CredibleInterval_0.95 = prior_quantile_0.95, 
                              
                              #posterior_alpha_Qual = posterior_alpha_Qual,
                              #posterior_beta_Qual = posterior_beta_Qual,  
                              #mode_posterior_Qual =  mode_posterior_Qual,
                              #mean_posterior_Qual = mean_posterior_Qual, 
                              #posterior_CredibleInterval_0.05_Qual = posterior_quantile_0.05_Qual, 
                              #posterior_CredibleInterval_0.95_Qual = posterior_quantile_0.95_Qual,
                              
                              
                              #Total_N_Construct = N, 
                              #Number_ofStudies_assessing_Construct = k, 
                              #Pooled_LOGOdds_Ratio = LOGOdds_Ratio,
                              #LowerCI_LogOddsRatio = LowerCI_LogOddsRatio, 
                              #UpperCI_LogOddsRatio = UpperCI_LogOddsRatio,
                              #N_PA_X = N_PA_X, 
                              
                              
                              posterior_alpha = posterior_alpha,
                              posterior_beta = posterior_beta,  
                              acceptance_Metropolis_MCMC = acceptance)))
                              #mode_posterior =  mode_posterior,
                              #mean_posterior = mean_posterior,
                              #posterior_CredibleInterval_0.05 = posterior_quantile_0.05, 
                              #posterior_CredibleInterval_0.95 = posterior_quantile_0.95)))
  
}
