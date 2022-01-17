
#this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 

library(ggplot2)
library(reshape2)  
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
###### (a) Hyperprior: the levels of physical activity in HF worldwide, general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013),
###### (b) the Prior elicited from the expert elicitation task which was based on the qualitative evidence; 
###### (c) Likelihood (pooled log OR estimates from the quantitative studies); 
###### (d) and Posterior (the posterior distribution obtained by means of Bayes update (first update Hyperprior with Prior, the update that with the Likelihood). Bayes Update is implimmmented per Spighelhalter et al., 2013 reccomendations) 
#reference: Spiegelhalter DJ, Abrams KR, Myles JP. Bayesian Approaches to Clinical Trials and Health-Care Evaluation. Chichester, UK: John Wiley & Sons, Ltd; 2003


BayesUpdateStepByStep <- function(x, Construct, uncertainty, seed) {
  
  #below we index the data by the name of construct
  index = x$Construct == Construct
  
  #data for the hyperprior: 
  JaarsmaInternationalStudy = JaarsmaInternationalStudy
 
  #variance and mean estimated from Jaarsma study (empirical hyperprior):    
  variance = 0.018
  mean = 0.43
  
  #calculate a and b parameters for the hyperprior distribution (which is a beta distribution, Spiegelhalter et al., 2003)
  
  HyperPrior_a = ((1 - mean) / variance - 1 / mean) * mean ^ 2
  HyperPrior_b = (HyperPrior_a * (1 /mean - 1))

  #the mean of the prior distriution of PA (HYPERPRIOR)
  mean_prior = HyperPrior_a/(HyperPrior_a+HyperPrior_b)
  
  #the mode of the prior distribution of PA (HYPERPRIOR) is
  mode_prior = (HyperPrior_a-1) / (HyperPrior_a+HyperPrior_b-2)
  
  #the variance of the prior distribution of PA (HYPERPRIOR) is
  variance_prior = (HyperPrior_a * HyperPrior_b) / ((HyperPrior_a+HyperPrior_b)^2*(HyperPrior_a+HyperPrior_b+1))
  

  #below we are sampling the entire beta distribution for the hyperprior given a and b parameters. 
  
  Theta = seq(0.01, 0.99, 0.01)
  prior_nonnormalised = dbeta(Theta,  HyperPrior_a,  HyperPrior_b, ncp = 0)
  PriorMean = mean(prior_nonnormalised)
  
  #the beta distribution is normalised below (following Spiegelhalter et al., 2003 reccomendations)
  prior = prior_nonnormalised/sum(prior_nonnormalised)
  Mean_normalised = mean(prior)
  
  
 #HYPERPRIOR Credible Iinterval are calculated below:
  
  p = rbeta(Theta, shape1 = HyperPrior_a, shape2 = HyperPrior_b, ncp = 0)
  q =  qbeta(p = p, shape1 = HyperPrior_a, shape2 = HyperPrior_b, ncp = 0)
  
  prior_quantile_0.05 = qbeta(0.05,  HyperPrior_a, HyperPrior_b)

  prior_quantile_0.95 = qbeta(0.95,  HyperPrior_a, HyperPrior_b,ncp = 0)
  
  MAP_hyperPrior = 0.422856163424018
  HDILower_hyperPrior =  0.220057520487314
  HDIUpper_hyperPrior =  0.640617186187267

  #mode 
  PriorMode = qbeta(0.5,  HyperPrior_a, HyperPrior_b,ncp = 0)


  #The prior_cumsum is below:
  density = data.frame(Theta, prior, prior_nonnormalised, PriorMean, PriorMode, prior_quantile_0.05, prior_quantile_0.95)
  prior_cumsum = cumsum(density$prior_nonnormalised)
  
  density = cbind(density, prior_cumsum)
  
  #The  CIs are below:
  prior_CI = ifelse(
    prior_cumsum<0.03|prior_cumsum>0.97,
    "outside CI",  "inside CI"
  )

  density = cbind(density, prior_CI)
  
  
  
  #calculate HYPERPRIOR Log Odds: 
  Variance_Prob_PA_JaarsmaInternationalStudy = 0.0189523150981809
  ProbPA_JaarsmaInternationalStudy = 0.433382295
  Prob_NoPA_JaarsmaInternationalStudy = 0.566617705
  
  Odds_prior = ((1- ProbPA_JaarsmaInternationalStudy)/ProbPA_JaarsmaInternationalStudy)/((1-Prob_NoPA_JaarsmaInternationalStudy)/Prob_NoPA_JaarsmaInternationalStudy)
  
  LogOdds_priorEstimate = log(ProbPA_JaarsmaInternationalStudy) + log(1-Prob_NoPA_JaarsmaInternationalStudy) - log(Prob_NoPA_JaarsmaInternationalStudy) - log(1-ProbPA_JaarsmaInternationalStudy)

  
  ProbabilityDistribution_Prior = pbeta(Theta, HyperPrior_a, HyperPrior_b, ncp = 0)
  ProbabilityDistribution_Prior = ProbabilityDistribution_Prior/sum(ProbabilityDistribution_Prior)
  PriorDistribution = data.frame(Theta, ProbabilityDistribution_Prior, PriorMean, PriorMode, prior_quantile_0.05, prior_quantile_0.95)
  

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

  #below we are updating Hyperprior  with Qualitative Results elicited from the expert elicitation task for each construct separately: 
  #formulas for calculating posterior beta and alpha parameters for the updated distribution (i.e., hyperprior with qualitative prior) below are from Spieghelhalter et al. 2003, p60: 
  
  posterior_alpha_Qual = HyperPrior_a + x[index,]$PriorExpert_N_PA_X 
  posterior_beta_Qual = HyperPrior_b + x[index,]$PriorExpert_N_noPA_X
  mean_posterior_Qual = posterior_alpha_Qual/(posterior_alpha_Qual+posterior_beta_Qual)
  mode_posterior_Qual =(posterior_alpha_Qual-1)/(posterior_alpha_Qual+posterior_beta_Qual-2)
  variance_posterior_Qual = (posterior_alpha_Qual * posterior_beta_Qual) / ((posterior_alpha_Qual+posterior_beta_Qual)^2*(posterior_alpha_Qual+posterior_beta_Qual+1))

  #Credible intervals: 
  posterior_quantile_0.05_Qual = qbeta(0.05, posterior_alpha_Qual,posterior_beta_Qual)
  posterior_quantile_0.95_Qual = qbeta(0.95, posterior_alpha_Qual,posterior_beta_Qual)
  posterior_mode_Qual = qbeta(0.5, posterior_alpha_Qual,posterior_beta_Qual)

  
  #the posterior distribution that combined hyperprior with prior: 
  posterior1_Qual = dbeta(Theta, posterior_alpha_Qual, posterior_beta_Qual)
  posterior1_normalised_Qual = posterior1_Qual/sum(posterior1_Qual)
  length(posterior1_Qual)
  length(Theta)
  plot(Theta, posterior1_Qual)
  density_posterior_Qual = data.frame(Theta, posterior1_Qual, mode_posterior_Qual, mean_posterior_Qual, posterior_quantile_0.05_Qual, posterior_quantile_0.95_Qual)
  
  graph_Posterior_Qual = plotDensity(data = density_posterior_Qual,
                                aes( x = density_posterior_Qual$Theta, 
                                     y = density_posterior_Qual$posterior1_Qual,
                                     fill = NULL),
                                mode = density_posterior_Qual$mode_posterior_Qual,
                                mean = density_posterior_Qual$mean_posterior_Qual, 
                                quantile_0.05 = density_posterior_Qual$posterior_quantile_0.05_Qual,
                                quantile_0.95 = density_posterior_Qual$posterior_quantile_0.95_Qual,
                                MAPhyperprior = MAP_hyperPrior,
                                CIUpperhyperprior = HDIUpper_hyperPrior, 
                                CILowerhyperprior = HDILower_hyperPrior, 
                                xlabTitle = paste("Probability of physical activity for", print(Construct)),  
                                ylabTitle = "Probability density", 
                                title = paste("Expert Belief: Probability of physical activity given", print(Construct)))
  
  print(graph_Posterior_Qual)
  

  
  ProbabilityDistribution_Posterior_Qual = qbeta(Theta, posterior_alpha_Qual, posterior_beta_Qual)
  PosteriorProbability_distribution_Qual = data.frame(Theta, 
                                                 ProbabilityDistribution_Posterior_Qual,
                                                 mode_posterior_Qual, 
                                                 mean_posterior_Qual, 
                                                 posterior_quantile_0.05_Qual,
                                                 posterior_quantile_0.95_Qual)
  
  
  
  graph_PosteriorProbability_distribution_Qual = plotDensity(data = PosteriorProbability_distribution_Qual,
                                                        aes( x = PosteriorProbability_distribution_Qual$Theta, 
                                                             y = PosteriorProbability_distribution_Qual$ProbabilityDistribution_Posterior_Qual,
                                                             fill = NULL),
                                                        mode = PosteriorProbability_distribution_Qual$mode_posterior_Qual,
                                                        mean = PosteriorProbability_distribution_Qual$mean_posterior_Qual, 
                                                        quantile_0.05 = PosteriorProbability_distribution_Qual$posterior_quantile_0.05_Qual,
                                                        quantile_0.95 = PosteriorProbability_distribution_Qual$posterior_quantile_0.95_Qual,
                                                        xlabTitle = paste("Posterior probability of physical activity for", print(Construct)),  
                                                        ylabTitle = "Probability distribution", 
                                                        title = paste("Expert Belief: Probability of physical activity given", print(Construct)))
  
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
  
  
  # the number of people who were physically active given a construct across the included studies was estimated using MCMC rejection sampling, with the rejection sampling where the simultanious rejection criteria were: 
  # 1) the contingency table has to add up to the Total N; 2) probabilities contingency table should add up to log OR 
  #estimate the contingencies table  using Monte Carlo Markov Chain rejection sampling (the total N and Log odds ratio formulas are used as the rejection criteria)

  
  #https://mc-stan.org/bayesplot/articles/plotting-mcmc-draws.html  source for plotting mcmc 
  

  
  expit<-function(x){exp(x)/(1+exp(x))}
  
  
  #ContingenciesTable = ContingenciesTable_MCMC(N = N, LOGOdds_Ratio = LOGOdds_Ratio)
  
  
  
  
  #N_noPA_noX = ContingenciesTable$N_noPA_noX
  #N_PA_X = ContingenciesTable$N_PA_X
  #N_noPA_X = ContingenciesTable$N_noPA_X
  #N_PA_noX =  ContingenciesTable$N_PA_noX
  
  #N_PA = ContingenciesTable$N_PA_X + ContingenciesTable$N_PA_noX 
  #N_X =  ContingenciesTable$N_noPA_X + ContingenciesTable$N_PA_X
  

  posterior_density = function(beta, 
                               #N_PA_X, 
                               #N_noPA_X, 
                               #N_PA_noX,     
                               #N_PA, 
                               #N_X, 
                               #N
                               LOGOdds_Ratio){
    
    
    probability_PA_X = expit(beta[2])
    
    #Vector_of_PA_X = rbinom(n = N_PA, size = 1, prob = probability_PA_X)
    
    #probability_PA_X_all = rlogis(n = N_PA_X, location = beta[1], scale =beta[2])
    #probability_PA_X_distribution  = plogis(q =probability_PA_X_all, location = beta[1], scale = beta[2], log.p = TRUE)
    #probability_PA_X_quantile = qlogis(probability_PA_X_distribution, location = beta[1], scale = beta[2], log.p = TRUE)
    
    #Likelihood_PA_X = sum(dbinom(x = Vector_of_PA_X, size = 1, prob = probability_PA_X, log = TRUE))
    Likelihood_PA_X = LOGOdds_Ratio
    
    PA_prior = sum(dnorm(beta, 0, uncertainty, log = TRUE))

    Posterior = Likelihood_PA_X + PA_prior
    return(params = list(probability_PA_X = probability_PA_X, Likelihood_PA_X = Likelihood_PA_X, beta = beta, Posterior = Posterior))
  }
  
  #The parameters for beta and alpha were sampled using Metropolis samling below: 
  #function for metropolis-hestings sampling, was adopted to this review from reference: https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/


  run_metropolis_MCMC = function(startingValues, iterations){
    samples = array(dim = c(iterations+1,2), dimnames = NULL)
    samples[1,] = startingValues
    
    
    for (i in 1:iterations){
     
      
      
      proposal = samples[i,] +  rnorm(n = 2, mean = 0, sd = 0.1)
      
      new_parameterList_likelihood = posterior_density(beta = proposal, 
                                         #N_PA_X = N_PA_X, 
                                         #N_noPA_X = N_noPA_X, 
                                         #N_PA_noX = N_PA_noX, 
                                         #N_PA = N_PA, 
                                         #N_X =  N_X, 
                                         #N = N
                                         LOGOdds_Ratio) 
    
      
      old_parameterList_likelihood = posterior_density(beta = samples[i,], 
                                         #N_PA_X = N_PA_X, 
                                         #N_noPA_X = N_noPA_X,
                                         #N_PA_noX = N_PA_noX, 
                                         #N_PA = N_PA, 
                                         #N_X =  N_X, 
                                         #N = N
                                         LOGOdds_Ratio)
      

      
      
      likelihood_ratio = new_parameterList_likelihood$Posterior - old_parameterList_likelihood$Posterior #convergence criterion 
 
      
      U <- runif(1)  
      if (U < likelihood_ratio){
        
        samples[i+1,] = proposal
        
        
      }else{
        samples[i+1,] = samples[i,]
      }
    }
    return(samples)} 
  
  
  # calculating Probability_PA_X below: 
  
  startingValues = c(0, 0.5)
  samples =  run_metropolis_MCMC(startingValues = startingValues, iterations = 100)
  burnIn = 50
  
  acceptance = 1-mean(duplicated(samples[-(1:burnIn),]))
  
  hist(samples[-(1:burnIn),1],nclass=30, main="Posterior of alpha, scale", xlab="alpha" )
  
  hist(samples[-(1:burnIn),2],nclass=30, main="Posterior of beta, location", xlab="beta" )
  
  
  a = mean(samples[-(1:burnIn),1])
  b = mean(samples[-(1:burnIn),2]) 
  
  
  # take the last 100 values from the mcmc object above which are values it converged on in the convernage MH sampling. Samples are an array of (ittirations, 2) 2 are alpha and beta 
  #taking the last 100 values for beta, and 100 values for alpha and taking a mean of it is going to be an accurate estimate of alpha and beta, due to a large burnin (50000)
  
  alpha_and_beta = tail(samples[-(1:burnIn),1], n=10)
  alpha = mean(alpha_and_beta[1])
  beta = mean(alpha_and_beta[2])
  probability_PA_X =beta
  

  #elicit the entire distribution: 
  
  #probability_PA_X_all = rlogis(n = N_PA_X, location = a, scale = b)
  #Theta = seq(0, 1, 1/N_PA_X)
  probability_PA_X_distribution  = plogis(q = Theta, location = alpha, scale = beta,lower.tail = FALSE, log.p = TRUE)
  MeanDistribution = mean(probability_PA_X_distribution)
  MeanDistribution_value = MeanDistribution
  #MeanDistribution = rep(MeanDistribution, times = N_PA_X+1)
  probability_PA_X_quantile = qlogis(p = probability_PA_X_distribution, location = beta, lower.tail = FALSE, scale = b, log.p = TRUE)
  
  #elicit density: 
  probability_PA_X_density = dlogis(x = Theta, location = alpha, scale = beta, log = TRUE)
  probability_PA_X_density_normalised = probability_PA_X_density/sum(probability_PA_X_density)
  probability_PA_X_density_expit = expit(probability_PA_X_density)
  probability_PA_X_Density_fromDATA_normalised = probability_PA_X_density_expit/sum(probability_PA_X_density_expit)
  MeanDensity = mean(probability_PA_X_density)
  MeanDensity_value = MeanDensity
  # = rep(MeanDensity, times =  N_PA_X+1)

  
  first_Quantilie_Density = qlogis(0.05,  alpha, beta)
  first_Quantilie_Density_value = first_Quantilie_Density
  #first_Quantilie_Density = rep(first_Quantilie_Density, times =  N_PA_X+1)
  
  second_Quantilie_Density = qlogis(0.95, alpha, beta)
  second_Quantilie_Density_value = second_Quantilie_Density
  #second_Quantilie_Density = rep(second_Quantilie_Density, times =  N_PA_X+1)
  
  ModeDensity = qlogis(0.5,  alpha, beta)
  ModeDensity_value = ModeDensity
  #ModeDensity = rep(ModeDensity, times =  N_PA_X+1)
  
  
  Prob_PA_X_DATA = data.frame(Theta, probability_PA_X_density, probability_PA_X_density_normalised, MeanDensity, ModeDensity, first_Quantilie_Density, second_Quantilie_Density)
  dim(Theta)
  dim(probability_PA_X_density)
  dim(MeanDensity)
  dim(ModeDensity)
  dim(first_Quantilie_Density)
  dim(second_Quantilie_Density)
  
  
  Prob_PA_X_DATA_cumsum = cumsum(probability_PA_X_density_normalised)
  likelihood_CI = ifelse(
  Prob_PA_X_DATA_cumsum<0.025|Prob_PA_X_DATA_cumsum>0.975,
  "outside CI","inside CI")
  Prob_PA_X_DATA = cbind(Prob_PA_X_DATA, likelihood_CI)

  #plot the density distribution: 
  graph_Likelihood_Probability_PA_X_density = plotDensity(data = Prob_PA_X_DATA,
                                                          aes(x = Prob_PA_X_DATA$Theta, 
                                                              y = Prob_PA_X_DATA$probability_PA_X_density,
                                                              fill = Prob_PA_X_DATA$likelihood_CI),
                                                          mean = MeanDensity_value, 
                                                          mode = ModeDensity_value,
                                                          MAPhyperprior = MAP_hyperPrior,
                                                          CIUpperhyperprior = HDIUpper_hyperPrior, 
                                                          CILowerhyperprior = HDILower_hyperPrior, 
                                                          quantile_0.05 = first_Quantilie_Density_value,
                                                          quantile_0.95 = second_Quantilie_Density_value,
                                                          xlabTitle = paste(print(sprintf("Joint probability density of physical activity and %s, pooled estimate from quantitative studies.", Construct))), 
                                                          ylabTitle = "Data: Probability Density", 
                                                          
                                                          title = Construct)
  print(graph_Likelihood_Probability_PA_X_density)
  

  Prob_PA_X_DATA_distribution = data.frame(Theta, probability_PA_X_distribution, MeanDistribution, ModeDensity, ModeDensity_value, first_Quantilie_Density, second_Quantilie_Density)
  Prob_PA_X_DATA_distribution_cumsum = cumsum(probability_PA_X_distribution)
  Distribution_CI = ifelse(
    Prob_PA_X_DATA_distribution_cumsum<0.025|Prob_PA_X_DATA_distribution_cumsum>0.975,
    "outside CI","inside CI")
  
  
  
  Prob_PA_X_DATA_distribution = cbind(Prob_PA_X_DATA_distribution, Distribution_CI)

  
  # concatinate the results for the likelihood distribution 
  #Likelihood_FromSampledBeta = data.frame(N_PA_X, 
                                          #probability_PA_X_expit, 
                                          #MeanDensity_value,
                                          #a,
                                          #b,
                                         # N_noPA_noX, 
                                         # N_noPA_X, 
                                         # N_PA_noX) 

  
  #below we update the prior with likelihood for each construct  
  #the parameters for the posterior below are  produced using Bayes update as specified by Spigielhalter et al. (2003) for beta-bernoulli distribution update (p60-62)

  N_PA_X = MeanDistribution_value * N
  posterior_alpha = posterior_alpha_Qual + N_PA_X
  posterior_beta = posterior_beta_Qual + N - N_PA_X
  mean_posterior = posterior_alpha/(posterior_alpha+posterior_beta)
  mode_posterior =(posterior_alpha-1)/(posterior_alpha+posterior_beta-2)
  variance_posterior = (posterior_alpha * posterior_beta) / ((posterior_alpha+posterior_beta)^2*(posterior_alpha+posterior_beta+1))

  
  #POSTERIOR Credible Intervals are estimated below: 
  posterior_quantile_0.05 = qbeta(0.05, posterior_alpha,posterior_beta)
  posterior_quantile_0.95 = qbeta(0.95, posterior_alpha,posterior_beta)

  posterior_mode = qbeta(0.5, posterior_alpha,posterior_beta)

  # elicit the entire posterior 
  posterior1 = dbeta(Theta, posterior_alpha, posterior_beta)
  posterior1_normalised = posterior1/sum(posterior1)
  
  #an alternative way of deriving the posterior is below, however, it will not print the plot syaing the finite value for y is required. The below is incorrect and is not reported but check with Spighelhalter again
  posterior3 = dbeta(Theta, HyperPrior_a * LOGOdds_Ratio, HyperPrior_b * (1/LOGOdds_Ratio))
  posterior3_normalised = posterior3/sum(posterior3)
  length(posterior1)
  length(Theta)
  
  #plot the posterior: 
  #plot(Theta, posterior1)
  
  #plot the posterior using ggplot: 
  
  density_posterior = data.frame(Theta, posterior1, mode_posterior, mean_posterior, posterior_quantile_0.05, posterior_quantile_0.95)
  graph_Posterior = plotDensity(data = density_posterior,
                                aes( x = density_posterior$Theta, 
                                     y = density_posterior$posterior1,
                                     fill = NULL),
                                mode = density_posterior$mode_posterior,
                                mean = density_posterior$mean_posterior, 
                                quantile_0.05 = density_posterior$posterior_quantile_0.05,
                                quantile_0.95 = density_posterior$posterior_quantile_0.95,
                                MAPhyperprior = MAP_hyperPrior,
                                CIUpperhyperprior = HDIUpper_hyperPrior, 
                                CILowerhyperprior = HDILower_hyperPrior, 
                                xlabTitle = paste("Posterior probability of physical activity for", print(Construct)),  
                                ylabTitle = "Probability density", 
                                title = Construct)
  

  
  print(graph_Posterior)
  
  ProbabilityDistribution_Posterior = qbeta(Theta, posterior_alpha, posterior_beta)
  PosteriorProbability_distribution = data.frame(Theta, 
                                                 ProbabilityDistribution_Posterior,
                                                 mode_posterior, 
                                                 mean_posterior, 
                                                 posterior_quantile_0.05,
                                                 posterior_quantile_0.95)
  
  
  return(params = (data.frame(Construct = Construct, 
                              prior_alpha = HyperPrior_a, 
                              prior_beta = HyperPrior_b, 
                              PriorMean = PriorMean, 
                              PriorMode = PriorMode,
                              prior_CredibleInterval_0.05 = prior_quantile_0.05, 
                              prior_CredibleInterval_0.95 = prior_quantile_0.95, 
                              
                              posterior_alpha_Qual = posterior_alpha_Qual,
                              posterior_beta_Qual = posterior_beta_Qual,  
                              mode_posterior_Qual =  mode_posterior_Qual,
                              mean_posterior_Qual = mean_posterior_Qual, 
                              posterior_CredibleInterval_0.05_Qual = posterior_quantile_0.05_Qual, 
                              posterior_CredibleInterval_0.95_Qual = posterior_quantile_0.95_Qual,
                              
                              
                              Total_N_Construct = N, 
                              Number_ofStudies_assessing_Construct = k, 
                              Pooled_LOGOdds_Ratio = LOGOdds_Ratio,
                              LowerCI_LogOddsRatio = LowerCI_LogOddsRatio, 
                              UpperCI_LogOddsRatio = UpperCI_LogOddsRatio,
                              N_PA_X = N_PA_X, 
                              
                              
                              posterior_alpha = posterior_alpha,
                              posterior_beta = posterior_beta,  
                              acceptance_Metropolis_MCMC = acceptance, 
                              mode_posterior =  mode_posterior,
                              mean_posterior = mean_posterior, 
                              posterior_CredibleInterval_0.05 = posterior_quantile_0.05, 
                              posterior_CredibleInterval_0.95 = posterior_quantile_0.95)))
  
}
