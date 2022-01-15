

# this function that runs Bayesian meta-analysis of quantitative evidence
## THE BAYES UPDATE WITHOUT THE PRIOR ELICITED FROM THE QUALITATIVE STUDIES
### Bayes update: Jaarsma's empirical hyperprior + quantitative studies 

#methods used here are identical to the methods used for the bayesian meta-analysis combining qualitative and quantitative studies, however here we omit the quanlitative evidence. 

library(ggplot2)
library(reshape2)  
library(tibble)
library(bayestestR)
library(HDInterval)

x = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/input.csv')  #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 

data = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/QuantData_CheckedForAccuracy_20March2020.csv')  #data extracted from from the quantitative studies 

JaarsmaInternationalStudy = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/HyperPriorData.csv') #empirical hyperprior 


source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/ConvertEffectsizes.R')

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/plbeta_function.R') 

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/PooledN.R') 

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/PooledOddsRatio_metaanalysis.R')

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/ContingenciesTable_MCMC.R')

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/Density_ggplot.R')

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/N_success.R')  


# this function below produces  density, distribution and all summary statistic for: 
#############################################################  (a) the prior: the levels of physical activity worldwide (Jaarsma et al., 2013); 
#############################################################  (b) the likelihood (pooled estimates from teh quantitative studies);
#############################################################  (c) and posterior (the bayes update implimmmented per Spighelhalter et al., 2004 reccomendation).
#function for computin prior, likelihood and posterior density. reference: https://rpubs.com/RRisto/betadist


BayesUpdate_Quant <- function(data, Construct, uncertainty, seed) {
  
  index = x$Construct == Construct
  
 #elicit hyperprior distribution from Jaarsm et al (2003) evidence: 
  
JaarsmaInternationalStudy = JaarsmaInternationalStudy
variance = 0.018
mean = 0.43

  HyperPrior_a = ((1 - mean) / variance - 1 / mean) * mean ^ 2
  HyperPrior_b = (HyperPrior_a * (1 /mean - 1))

  
  mean_prior = HyperPrior_a/(HyperPrior_a+HyperPrior_b)
  mode_prior = (HyperPrior_a-1) / (HyperPrior_a+HyperPrior_b-2)
  variance_prior = (HyperPrior_a * HyperPrior_b) / ((HyperPrior_a+HyperPrior_b)^2*(HyperPrior_a+HyperPrior_b+1))
 
  
  Theta = seq(0.01, 0.99, 0.01)
  prior_nonnormalised = dbeta(Theta,  HyperPrior_a,  HyperPrior_b, ncp = 0,  log = FALSE)
  PriorMean = mean(prior_nonnormalised)
  
  prior = prior_nonnormalised/sum(prior_nonnormalised)
  Mean_normalised = mean(prior)

  #HYPERPRIOR Credible Iinterval start below: 
  p = rbeta(Theta, shape1 = HyperPrior_a, shape2 = HyperPrior_b, ncp = 0)
  MAP_hyperPrior = 0.422856163424018
  HDILower_hyperPrior =  0.220057520487314
  HDIUpper_hyperPrior =  0.640617186187267
  
  
  q =  qbeta(p = p, shape1 = HyperPrior_a, shape2 = HyperPrior_b, ncp = 0,  log = FALSE)
  
  prior_quantile_0.05 = qbeta(0.05,  HyperPrior_a, HyperPrior_b)
  prior_quantile_0.95 = qbeta(0.95,  HyperPrior_a, HyperPrior_b,ncp = 0, log = FALSE)
  PriorMode = qbeta(0.5,  HyperPrior_a, HyperPrior_b,ncp = 0)

  
  density = data.frame(Theta, prior, prior_nonnormalised)
  prior_cumsum = cumsum(density$prior_nonnormalised)

  density = cbind(density, prior_cumsum)
  
  prior_CI = ifelse(
    prior_cumsum<0.03|prior_cumsum>0.97,
    "outside CI",  "inside CI"
  )

  density = cbind(density, prior_CI)
  


  
  #HYPERPRIOR Log Odds:
  
  Variance_Prob_PA_JaarsmaInternationalStudy = 0.0189523150981809
  ProbPA_JaarsmaInternationalStudy = 0.433382295
  Prob_NoPA_JaarsmaInternationalStudy = 0.566617705
  
  
  Odds_prior = ((1- ProbPA_JaarsmaInternationalStudy)/ProbPA_JaarsmaInternationalStudy)/((1-Prob_NoPA_JaarsmaInternationalStudy)/Prob_NoPA_JaarsmaInternationalStudy)
  
  LogOdds_priorEstimate = log(ProbPA_JaarsmaInternationalStudy) + log(1-Prob_NoPA_JaarsmaInternationalStudy) - log(Prob_NoPA_JaarsmaInternationalStudy) - log(1-ProbPA_JaarsmaInternationalStudy)

  
 ProbabilityDistribution_Prior = pbeta(Theta, HyperPrior_a, HyperPrior_b, ncp = 0)
  ProbabilityDistribution_Prior = ProbabilityDistribution_Prior/sum(ProbabilityDistribution_Prior)
 
  
  
#TOTAL N across quantitative studies evaluating each construct:

  PooledN_output = PooledN(data = data, Construct = Construct)
  N = PooledN_output$N

  
 #Odds Ratio Pooled OR across Quant Studies (method: metafor library): 
  
  meta_data_likelihoodResults = metaDataLikelihood(likelihood_data = likelihood_data, Construct = Construct)
  LOGOdds_Ratio = meta_data_likelihoodResults$LOGOdds_Ratio
  LowerCI_LogOddsRatio = meta_data_likelihoodResults$LowerCI_LogOddsRatio 
  UpperCI_LogOddsRatio = meta_data_likelihoodResults$UpperCI_LogOddsRatio
  k =  meta_data_likelihoodResults$k
  
  
 #LIKELIIHOOD: 
  
  #https://mc-stan.org/bayesplot/articles/plotting-mcmc-draws.html  plotting mcmc 
  tolerance = 1e-5
  nmc = 1e+7
  
  expit<-function(x){exp(x)/(1+exp(x))}
  
  
  ContingenciesTable = ContingenciesTable_MCMC(N = N, LOGOdds_Ratio = LOGOdds_Ratio)
  N_noPA_noX = ContingenciesTable$N_noPA_noX
  N_PA_X = ContingenciesTable$N_PA_X
  N_noPA_X = ContingenciesTable$N_noPA_X
  N_PA_noX =  ContingenciesTable$N_PA_noX
  N_PA = ContingenciesTable$N_PA_X + ContingenciesTable$N_PA_noX 
  N_X =  ContingenciesTable$N_noPA_X + ContingenciesTable$N_PA_X
  

  
  #POSTERIOR: 
  posterior_alpha = HyperPrior_a + N_PA_X
  posterior_beta = HyperPrior_b + N - N_PA_X

  Posterior_dis = dbeta(Theta, HyperPrior_a, HyperPrior_b)
  MAP_posterior = map_estimate(Posterior_dis)
  HDI_Posterior= hdi(Posterior_dis, credMass = 0.90)
  HDILower_Posterior = HDI_Posterior[[1]]
  HDIUpper_Posterior = HDI_Posterior[[2]]
  mean_posterior = posterior_alpha/(posterior_alpha+posterior_beta)
  mode_posterior =(posterior_alpha-1)/(posterior_alpha+posterior_beta-2)
  variance_posterior = (posterior_alpha * posterior_beta) / ((posterior_alpha+posterior_beta)^2*(posterior_alpha+posterior_beta+1))
  
  #POSTERIOR Credible Interval start below: 
  posterior_quantile_0.05 = qbeta(0.05, posterior_alpha,posterior_beta)
  posterior_quantile_0.95 = qbeta(0.95, posterior_alpha,posterior_beta)

  
  #POSTERIOR distribution: 
  posterior_mode = qbeta(0.5, posterior_alpha,posterior_beta)
  posterior1 = dbeta(Theta, posterior_alpha, posterior_beta)
  posterior1_normalised = posterior1/sum(posterior1)
  
  PosteriorMean = mean(posterior1)
  PosteriorMean_normalised = mean(posterior1_normalised)
  posterior3 = dbeta(Theta, HyperPrior_a * LOGOdds_Ratio, HyperPrior_b * (1/LOGOdds_Ratio))
  posterior3_normalised = posterior3/sum(posterior3)
  length(posterior1)
  length(Theta)
  plot(Theta, posterior1)

  #plot posterior distribution using ggplot: 
  
  density_posterior = data.frame(Theta, posterior1)
  graph_Posterior = plotDensity(data = density_posterior,
                                aes( x = density_posterior$Theta, 
                                     y = density_posterior$posterior1,
                                     fill = NULL),
                                mode = mode_posterior,
                                mean = mean_posterior, 
                                quantile_0.05 = posterior_quantile_0.05,
                                quantile_0.95 = posterior_quantile_0.95,
                                MAPhyperprior = MAP_hyperPrior, 
                                CIUpperhyperprior = HDIUpper_hyperPrior, 
                                CILowerhyperprior = HDILower_hyperPrior, 
                                xlabTitle = paste("Posterior probability of physical activity for", print(Construct)),  
                                ylabTitle = "Probability density", 
                                title = Construct)
  

  print(graph_Posterior)
  


  return(params = (list(Construct = Construct, 
                        prior_alpha = HyperPrior_a, 
                        prior_beta = HyperPrior_b, 
                        mean_prior = mean_prior,
                        PriorMean = PriorMean,
                        Mean_normalised = Mean_normalised,
                        PriorMode = PriorMode,
                        Odds_prior = Odds_prior, 
                        LogOdds_priorEstimate = LogOdds_priorEstimate, 
                        prior_CredibleInterval_0.05 = prior_quantile_0.05, 
                        prior_CredibleInterval_0.95 = prior_quantile_0.95, 
                        MAP_hyperPrior = MAP_hyperPrior,
                        HDILower_hyperPrior = HDILower_hyperPrior,
                        HDIUpper_hyperPrior = HDIUpper_hyperPrior,
                        Total_N_Construct = N, 
                        Number_ofStudies_assessing_Construct = k, 
                        Pooled_LOGOdds_Ratio = LOGOdds_Ratio,
                        LowerCI_LogOddsRatio = LowerCI_LogOddsRatio, 
                        UpperCI_LogOddsRatio = UpperCI_LogOddsRatio,
                        posterior_alpha = posterior_alpha,
                        posterior_beta = posterior_beta,  
                        MAP_posterior = MAP_posterior,
                        HDILower_Posterior = HDILower_Posterior,
                        HDIUpper_Posterior = HDIUpper_Posterior,
                        mode_posterior =  mode_posterior,
                        mean_posterior = mean_posterior, 
                        PosteriorMean = PosteriorMean, 
                        PosteriorMean_normalised = PosteriorMean_normalised, 
                        posterior_CredibleInterval_0.05 = posterior_quantile_0.05, 
                        posterior_CredibleInterval_0.95 = posterior_quantile_0.95)))
  
}


