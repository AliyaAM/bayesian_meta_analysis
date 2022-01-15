
library(ggplot2)
library(reshape2)  
library(tibble)
library(bayestestR)
library(HDInterval)


data = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/QuantData_CheckedForAccuracy_20March2020.csv') 
print(data)


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

# using bayesmeta R function. 
#given the alpha and beta paramenters, N, and Odds Ratio. 
# for each Construct, estimate alpha and beta parameters from median and variance from the qualitative prior using estBetaParams function
# here we use the median based on experts elicitaiton task,  I estimate for m, and sample variance for variance, 
#this is only true if we only care about sample variance an assume that this variance is representative of a true variance 
#but also if we only care about the answers provided by the sample)

BayesUpdate_Quant <- function(data, Construct, uncertainty, seed) {
  
  index = data$Construct == Construct

  
  print("started running Bayes update of the Quant Studies")
  
  print("****************************************** Bayesian meta-analysis (QUANTATITIVE) results for *************************************************************")
  
  print(Construct)  
  
  print("*********************************************** HYPERPRIOR *************************************************************************************")
  
  JaarsmaInternationalStudy = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/HyperPriorData.csv') 
  
  variance = 0.018
  mean = 0.43

  #HyperPrior_a = mean*variance
  #HyperPrior_b = (1-mean)*variance
  #print(HyperPrior_a)
  #print(HyperPrior_b)
  
  HyperPrior_a = ((1 - mean) / variance - 1 / mean) * mean ^ 2
  print(HyperPrior_a)
  HyperPrior_b = (HyperPrior_a * (1 /mean - 1))
  print(HyperPrior_b)
  
  
 #N_PA_X = 3305
  #a_flat = 0.5
  #b_flat =0.5
  
  #HyperPrior_a = 0.5+3305
  #HyperPrior_b = 0.5 + 7625 -3305
  
  
  
 # print("got HyperPrior_a")
  #print(HyperPrior_a)
 # print("got HyperPrior_b")
  #print(HyperPrior_b)
  
  mean_prior = HyperPrior_a/(HyperPrior_a+HyperPrior_b)
  mode_prior = (HyperPrior_a-1) / (HyperPrior_a+HyperPrior_b-2)
  variance_prior = (HyperPrior_a * HyperPrior_b) / ((HyperPrior_a+HyperPrior_b)^2*(HyperPrior_a+HyperPrior_b+1))
  
  print(Construct)
  
  print("alpha parameter for the beta distribution of PA (HYPERPRIOR) is")
  print(HyperPrior_a)
  
  print("beta parameter for the beta distribution of of PA (HYPERPRIOR) is")
  print(HyperPrior_b)
  print("given the parameters a and b, the mean of the prior distriution of PA (HYPERPRIOR) is")
  print(mean_prior)
  
  print("given the parameters a and b, the mode of the prior distribution of PA (HYPERPRIOR) is")
  print(mode_prior) 
  
  print("given the parameters a and b, the variance of the prior distribution of PA (HYPERPRIOR) is")
  print(variance_prior)
  
  
  Theta = seq(0.01, 0.99, 0.01)
  prior_nonnormalised = dbeta(Theta,  HyperPrior_a,  HyperPrior_b, ncp = 0,  log = FALSE)
  print("prior mean")
  PriorMean = mean(prior_nonnormalised)
  
  prior = prior_nonnormalised/sum(prior_nonnormalised)
  print("Prior for the Construct: ")
  print(Construct)
  print(prior_nonnormalised)
  print("prior mean normalised")
  Mean_normalised = mean(prior)
  print(Mean_normalised)
  
  print("*********************************************************************HYPERPRIOR Credible Iinterval start below: ")
  
  
  print("quantile_0.05 is/ 95 % Lower Credible Interval is")
  p = rbeta(Theta, shape1 = HyperPrior_a, shape2 = HyperPrior_b, ncp = 0)
  #calculated and then estimated the MAP for hyperprior using formulas below. Took an average over 38 guesses: 
  #MAP_hyperPrior = map_estimate(p)
  #print(MAP_hyperPrior)
  #HDI = hdi(p, credMass = 0.90)
  #HDILower_hyperPrior = HDI[[1]]
  #HDIUpper_hyperPrior = HDI[[2]]
  
  #the new value for MAP and CI is: 
  MAP_hyperPrior = 0.422856163424018
  HDILower_hyperPrior =  0.220057520487314
  HDIUpper_hyperPrior =  0.640617186187267
  
  
  q =  qbeta(p = p, shape1 = HyperPrior_a, shape2 = HyperPrior_b, ncp = 0,  log = FALSE)
  
  prior_quantile_0.05 = qbeta(0.05,  HyperPrior_a, HyperPrior_b)
  print(prior_quantile_0.05)
  
  print("quantile_0.95 is / 95 % Upper Credible Interval is")
  prior_quantile_0.95 = qbeta(0.95,  HyperPrior_a, HyperPrior_b,ncp = 0, log = FALSE)
  print(prior_quantile_0.95)
  
  print("prior mode is")
  PriorMode = qbeta(0.5,  HyperPrior_a, HyperPrior_b,ncp = 0)
  print(PriorMode)
  
  print("90% chance that the value is between") 
  print(prior_quantile_0.05) 
  print("and") 
  print(prior_quantile_0.95)
  
  print("***************************************************************************HYPERPRIOR Credible Interval ended: ")
  
  
  density = data.frame(Theta, prior, prior_nonnormalised)
  print(density)
  prior_cumsum = cumsum(density$prior_nonnormalised)
  print("The prior_cumsum is below:")
  print(prior_cumsum)
  
  density = cbind(density, prior_cumsum)
  
  print("The prior_CI is below:")
  prior_CI = ifelse(
    prior_cumsum<0.03|prior_cumsum>0.97,
    "outside CI",  "inside CI"
  )
  print(prior_CI)
  
  density = cbind(density, prior_CI)
  
  print("Got prior_Confidence Intervals")
  print("for Cosntruct")
  print(Construct)
  print(density)

  
  
  
  print("**************************************************HYPERPRIOR Log Odds *******************************************")
  
  Variance_Prob_PA_JaarsmaInternationalStudy = 0.0189523150981809
  ProbPA_JaarsmaInternationalStudy = 0.433382295
  Prob_NoPA_JaarsmaInternationalStudy = 0.566617705
  
  
  Odds_prior = ((1- ProbPA_JaarsmaInternationalStudy)/ProbPA_JaarsmaInternationalStudy)/((1-Prob_NoPA_JaarsmaInternationalStudy)/Prob_NoPA_JaarsmaInternationalStudy)
  
  LogOdds_priorEstimate = log(ProbPA_JaarsmaInternationalStudy) + log(1-Prob_NoPA_JaarsmaInternationalStudy) - log(Prob_NoPA_JaarsmaInternationalStudy) - log(1-ProbPA_JaarsmaInternationalStudy)

  #graph_Prior_Density = plotDensity(data = density,
   #                                 aes( x = density$Theta, 
  #                                    y = density$prior_nonnormalised,
  #                                       fill = NULL),
  #                                 mean = mean_prior, 
  #                                 mode = PriorMode,
  #                                 quantile_0.05 = prior_quantile_0.05,
  #                                 quantile_0.95 = prior_quantile_0.95,
  #                                MAPhyperprior = MAP_hyperPrior, 
  #                               CIUpperhyperprior = HDILower_hyperPrior, 
  #                                CILowerhyperprior = HDILower_hyperPrior,
  #                               xlabTitle = paste("Hyperprior probability of physical activity"), 
  #                                ylabTitle = "Probability density", 
                                   
  #                                 title = "Jaarsma et al 2013: Physical activity levels in HF")
  #print(graph_Prior_Density)
  
 ProbabilityDistribution_Prior = pbeta(Theta, HyperPrior_a, HyperPrior_b, ncp = 0)
  ProbabilityDistribution_Prior = ProbabilityDistribution_Prior/sum(ProbabilityDistribution_Prior)
  #PriorDistribution = data.frame(Theta, ProbabilityDistribution_Prior, PriorMean, PriorMode, prior_quantile_0.05, prior_quantile_0.95, mean_prior)
  
  
  
  #graph_Prior_Distribution = plotDensity(data = PriorDistribution,
   #                                     aes( x = PriorDistribution$Theta, 
  #                                           y = PriorDistribution$ProbabilityDistribution_Prior,
  #                                            fill = NULL),
  #                                       mean = PriorDistribution$mean_prior, 
  #                                      mode = PriorDistribution$PriorMode,
  #                                      quantile_0.05 = PriorDistribution$prior_quantile_0.05,
  #                                      quantile_0.95 = PriorDistribution$prior_quantile_0.95,
  #                                      xlabTitle = paste("Hyperprior probability of physical activity"), 
  #                                      ylabTitle = "Probability distirbution", 
  #                                      title = "Jaarsma et al 2013: Physical activity levels in HF")
  #print(graph_Prior_Distribution)
  
  
  # print prior  dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b) in one graph
  
  
  print("Finished computing the HYPERprior distribution for Construct:")
  print(Construct)

  
  print("****************************************** TOTAL N across quanti studies ******************************************************************")
  
  
  print("Calculate total N for this Construct:")
  print(Construct)
  PooledN_output = PooledN(data = data, Construct = Construct)
  print("N is")
  N = PooledN_output$N
  print("total N for this construct across the included studies =")
  print(N)
  
  print("****************************************** Odds Ratio Pooled OR across Quant Studies ******************************************************")
  
  print("Odds Ratio for this Construct:")
  print(Construct)
  
  #Pooled_ZCOR_RandomEffect_knha = meta_data_likelihood$Pooled_ZCOR_RandomEffect_knhaEstimate 
  
  
  meta_data_likelihoodResults = metaDataLikelihood(likelihood_data = likelihood_data, Construct = Construct)
  LOGOdds_Ratio = meta_data_likelihoodResults$LOGOdds_Ratio
  LowerCI_LogOddsRatio = meta_data_likelihoodResults$LowerCI_LogOddsRatio 
  UpperCI_LogOddsRatio = meta_data_likelihoodResults$UpperCI_LogOddsRatio
  
  print("log OR is")
  print(LOGOdds_Ratio)
  
  print("number of studies is")
  k =  meta_data_likelihoodResults$k
  
  
  print("**************************************** LIKELIIHOOD ***************************************************************************************")
  
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
  
  print(N)
  print(N_PA)
  print(N_PA_X)
  print(N_PA_noX)
  print(N_noPA_X)
  
  print(sprintf("the total N is: %s, N_PA: %s ||| N_X: %s ||| N_PA_X: %s  ||| N_PAnoX:  %s, N_noPA_X: %s", N, N_PA, N_X ,N_PA_X, N_PA_noX, N_noPA_X))
  
  #If it gets down to assert false then it did not find probability PA given X and should crash
  
  
  print("****************************************** POSTERIOR ****************************************************************************************")
  
  
  print(Construct)
  
  
  print("posterior alpha")
  posterior_alpha = HyperPrior_a + N_PA_X
  print(posterior_alpha)
  
  print("posterior beta")
  posterior_beta = HyperPrior_b + N - N_PA_X
  print(posterior_beta)
  
  Posterior_dis = dbeta(Theta, HyperPrior_a, HyperPrior_b)
  MAP_posterior = map_estimate(Posterior_dis)
  HDI_Posterior= hdi(Posterior_dis, credMass = 0.90)
  HDILower_Posterior = HDI_Posterior[[1]]
  HDIUpper_Posterior = HDI_Posterior[[2]]
  
  
  print("posterior Mean: ")
  
  mean_posterior = posterior_alpha/(posterior_alpha+posterior_beta)
  print(mean_posterior)
  
  print(" posteriorMode")
  mode_posterior =(posterior_alpha-1)/(posterior_alpha+posterior_beta-2)
  print(mode_posterior)
  
  print("posterior variance")
  variance_posterior = (posterior_alpha * posterior_beta) / ((posterior_alpha+posterior_beta)^2*(posterior_alpha+posterior_beta+1))
  print(variance_posterior)
  
  
  #Five Percentchance below 90% Credible interval: 
  print("*********************************************************************POSTERIOR Credible Interval start below: ")
  
  print("quantile_0.05 is/ 95 % Lower Credible Interval is")
  posterior_quantile_0.05 = qbeta(0.05, posterior_alpha,posterior_beta)
  print(posterior_quantile_0.05)
  
  print("quantile_0.95 is / 95 % Upper Credible Interval is")
  posterior_quantile_0.95 = qbeta(0.95, posterior_alpha,posterior_beta)
  print(posterior_quantile_0.95)
  
  print("posterior mode is (experiment if this actually conferges with the above")
  posterior_mode = qbeta(0.5, posterior_alpha,posterior_beta)
  print(posterior_mode)
  
  print("90% chance that the value is between") 
  print(posterior_quantile_0.05) 
  print("and") 
  print(posterior_quantile_0.95)
  
  
  print("***************************************************************************POSTERIOR Credible Interval ended: ")
  
  posterior1 = dbeta(Theta, posterior_alpha, posterior_beta)

  posterior1_normalised = posterior1/sum(posterior1)
  
  PosteriorMean = mean(posterior1)
  PosteriorMean_normalised = mean(posterior1_normalised)
  #posterior1=  dbeta(Theta, HyperPrior_a, HyperPrior_b) * dbeta(Theta, posterior_alpha, posterior_beta)
  
  #an alternative way of deriving the posterior is below, however, it will not print the plot syaing the finite value for y is required. 
  
  posterior3 = dbeta(Theta, HyperPrior_a * LOGOdds_Ratio, HyperPrior_b * (1/LOGOdds_Ratio))
  posterior3_normalised = posterior3/sum(posterior3)
  
 length(posterior1)
  length(Theta)
  
  
  plot(Theta, posterior1)

  
  print("plotting graph Posterior density distribution")
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
  
  ProbabilityDistribution_Posterior = pbeta(Theta, posterior_alpha, posterior_beta)
  ProbabilityDistribution_Posterior_normalised = ProbabilityDistribution_Posterior/sum(ProbabilityDistribution_Posterior)
  
  #PosteriorProbability_distribution = data.frame(Theta, 
  #                                               ProbabilityDistribution_Posterior_normalised,
  #                                               mode_posterior, 
  #                                               mean_posterior, 
  #                                               posterior_quantile_0.05,
  #                                               posterior_quantile_0.95)
  
  
  
  #graph_PosteriorProbability_distribution = plotDensity(data = PosteriorProbability_distribution,
   #                                                     aes( x = PosteriorProbability_distribution$Theta, 
  #                                                           y = PosteriorProbability_distribution$ProbabilityDistribution_Posterior_normalised,
  #                                                           fill = NULL),
  #                                                      mode = PosteriorProbability_distribution$mode_posterior,
  #                                                      mean = PosteriorProbability_distribution$mean_posterior, 
  #                                                      quantile_0.05 = PosteriorProbability_distribution$posterior_quantile_0.05,
  #                                                     quantile_0.95 = PosteriorProbability_distribution$posterior_quantile_0.95,
  #                                                      xlabTitle = paste("Posterior probability of physical activity conditioned on", print(Construct)),  
   #                                                     ylabTitle = "Probability distribution", 
   #                                                     title = Construct)
  
  #print(graph_PosteriorProbability_distribution)
  return(params = (data.frame(Construct = Construct, 
                              prior_alpha = HyperPrior_a, 
                              prior_beta = HyperPrior_b, 
                              mean_prior = mean_prior,
                              PriorMean = PriorMean,
                              Mean_normalised =Mean_normalised,
                              PriorMode = PriorMode,
                              Odds_prior = Odds_prior, 
                              LogOdds_priorEstimate = LogOdds_priorEstimate, 
                              #distribution, 
                              prior_CredibleInterval_0.05 = prior_quantile_0.05, 
                              prior_CredibleInterval_0.95 = prior_quantile_0.95, 
                              MAP_hyperPrior = MAP_hyperPrior,
                              HDILower_hyperPrior = HDILower_hyperPrior,
                              HDIUpper_hyperPrior = HDIUpper_hyperPrior,
                              Total_N_Construct = N, 
                              Number_ofStudies_assessing_Construct = k, 
                              #Pooled_ZCOR_RandomEffect_knha = Pooled_ZCOR_RandomEffect_knha,
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



#function for computin prior, likelihood and posterior density. reference: https://rpubs.com/RRisto/betadist



