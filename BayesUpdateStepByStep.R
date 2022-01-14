


library(ggplot2)
library(reshape2)  
library(tibble)

x = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/input.csv')
print(x)


data = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/QuantData_CheckedForAccuracy_20March2020.csv') 
print(data)

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/ConvertEffectsizes.R')   #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratio

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/PooledN.R') # calculate the total sample size (N) for the construct across studies, read from the csv file QuantData

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/PooledOddsRatio_metaanalysis.R')  #run the frequentisit (REML) meta-analysis of quant data. the Overall Effect estimate is (log) Odds Ratio  (the pooled Log OR for each Construct)

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/ContingenciesTable_MCMC.R') #estimate the contingencies table for each study using Monte Carlo Markov Chain rejection sampling (the total N and Log odds ratio formulas are used as the rejection criteria)

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/Density_ggplot.R') #produce illustrations: plot density and distribution of probabality of physical activity happening given the construct. 

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/N_success.R')  #calculate the proportion of total sample engaging in physical activity when construct present





# this function below produces distribution density for the: 
#############################################################  (a) hyperprior: the levels of physical activity worldwide (Jaarsma et al., 2013); 
#############################################################  (b) the prior elicited from the experts' task;
#############################################################  (c) the likelihood (pooled estimates from teh quantitative studies); 
#############################################################  (d) and posterior (the bayes update implimmmented per Spighelhalter et al., 2013 reccomendation).

# using bayesmeta R function. 
#given the alpha and beta paramenters, N, and Odds Ratio. 
# for each Construct, estimate alpha and beta parameters from median and variance from the qualitative prior using estBetaParams function
# here we use the median based on experts elicitaiton task,  I estimate for m, and sample variance for variance, 
#this is only true if we only care about sample variance an assume that this variance is representative of a true variance 
#but also if we only care about the answers provided by the sample)


BayesUpdateStepByStep <- function(x, Construct, uncertainty, seed) {
  
  print(uncertainty)
  
  index = x$Construct == Construct
  
  print("started running BayesBeta")
  
  print("****************************************** Bayesian meta-analysis results for *************************************************************")
  
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
  

  
  print("got HyperPrior_a")
  print(HyperPrior_a)
  print("got HyperPrior_b")
  print(HyperPrior_b)
  
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
  prior_nonnormalised = dbeta(Theta,  HyperPrior_a,  HyperPrior_b, ncp = 0)
  print("prior mean")
  PriorMean = mean(prior_nonnormalised)
  
  prior = prior_nonnormalised/sum(prior_nonnormalised)
  print("Prior for the Construct: ")
  print(Construct)
  print(prior_nonnormalised)
  print("prior mean normalised")
  Mean_normalised = mean(prior)
  
  
  print("*********************************************************************HYPERPRIOR Credible Iinterval start below: ")
  
  
  print("quantile_0.05 is/ 95 % Lower Credible Interval is")
  p = rbeta(Theta, shape1 = HyperPrior_a, shape2 = HyperPrior_b, ncp = 0)
  q =  qbeta(p = p, shape1 = HyperPrior_a, shape2 = HyperPrior_b, ncp = 0)
  
  prior_quantile_0.05 = qbeta(0.05,  HyperPrior_a, HyperPrior_b)
  print(prior_quantile_0.05)
  
  print("quantile_0.95 is / 95 % Upper Credible Interval is")
  prior_quantile_0.95 = qbeta(0.95,  HyperPrior_a, HyperPrior_b,ncp = 0)
  print(prior_quantile_0.95)
  
  print("prior mode is")
  PriorMode = qbeta(0.5,  HyperPrior_a, HyperPrior_b,ncp = 0)
  print(PriorMode)
  
  print("90% chance that the value is between") 
  print(prior_quantile_0.05) 
  print("and") 
  print(prior_quantile_0.95)
  
  print("***************************************************************************HYPERPRIOR Credible Interval ended: ")
  
  
  density = data.frame(Theta, prior, prior_nonnormalised, PriorMean, PriorMode, prior_quantile_0.05, prior_quantile_0.95)
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
  #Plotting the dnesities using the funciton called plotDensity
  
  #graph_Prior_Density = plotDensity(data = density,
  #                                  aes( x = density$Theta, 
  #                                       y = density$prior_nonnormalised,
  #                                       fill = density$prior_CI),
  #                                  mean = density$PriorMean, 
  #                                  mode = density$PriorMode,
  #                                  quantile_0.05 = density$prior_quantile_0.05,
  #                                  quantile_0.95 = density$prior_quantile_0.95,
  #                                  xlabTitle = paste("Hyperprior probability of physical activity"), 
  #                                  ylabTitle = "Probability density", 
  #                                  
  #                                  title = "Jaarsma et al 2013: Physical activity levels in HF")
  #print(graph_Prior_Density)
  
  ProbabilityDistribution_Prior = pbeta(Theta, HyperPrior_a, HyperPrior_b, ncp = 0)
  ProbabilityDistribution_Prior = ProbabilityDistribution_Prior/sum(ProbabilityDistribution_Prior)
  PriorDistribution = data.frame(Theta, ProbabilityDistribution_Prior, PriorMean, PriorMode, prior_quantile_0.05, prior_quantile_0.95)
  
  
  
 # graph_Prior_Distribution = plotDensity(data = PriorDistribution,
  #                                       aes( x = PriorDistribution$Theta, 
  #                                            y = PriorDistribution$ProbabilityDistribution_Prior,
  #                                            fill = NULL),
  #                                       mean = PriorDistribution$PriorMean, 
  #                                       mode = PriorDistribution$PriorMode,
  #                                       quantile_0.05 = PriorDistribution$prior_quantile_0.05,
  #                                       quantile_0.95 = PriorDistribution$prior_quantile_0.95,
  #                                       xlabTitle = paste("Hyperprior probability of physical activity"), 
  #                                       ylabTitle = "Probability distirbution", 
  #                                       title = "Jaarsma et al 2013: Physical activity levels in HF")
#  print(graph_Prior_Distribution)
  
  
  # print prior  dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b) in one graph later
  
  
  print("Finished computing the HYPERprior distribution for Construct:")
  print(Construct)
  print("************************ Prior number of successes ******************************")
  
  
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
  print(Number_successes)
  print(x)
  x = cbind(x, Number_successes)
  print(x)
  
  print("*********************************Hyper prior Update with Qualitative Results ***************************************************************")
  
  print(Construct)
  
  #formulas for calulating posterior beta and alpha parameters for the updated distribution (i.,e hyperprior with qualitative prior) below are from Spieghelhalter et al 2003 
  
  print("posterior alpha")
  posterior_alpha_Qual = HyperPrior_a + x[index,]$PriorExpert_N_PA_X 
  print(posterior_alpha_Qual)
  
  print("posterior beta")
  posterior_beta_Qual = HyperPrior_b + x[index,]$PriorExpert_N_noPA_X
  print(posterior_beta_Qual)
  
  print("posterior Mean: ")
  
  mean_posterior_Qual = posterior_alpha_Qual/(posterior_alpha_Qual+posterior_beta_Qual)
  print(mean_posterior_Qual)
  
  print(" posteriorMode")
  mode_posterior_Qual =(posterior_alpha_Qual-1)/(posterior_alpha_Qual+posterior_beta_Qual-2)
  print(mode_posterior_Qual)
  
  print("posterior variance")
  variance_posterior_Qual = (posterior_alpha_Qual * posterior_beta_Qual) / ((posterior_alpha_Qual+posterior_beta_Qual)^2*(posterior_alpha_Qual+posterior_beta_Qual+1))
  print(variance_posterior_Qual)
  
  
  #Five Percentchance below 90% Credible interval: 
  print("*********************************************************************Hyper prior Update with Qualitative Results Credible Interval start below: ")
  print("quantile_0.05 is/ 95 % Lower Credible Interval is")
  posterior_quantile_0.05_Qual = qbeta(0.05, posterior_alpha_Qual,posterior_beta_Qual)
  print(posterior_quantile_0.05_Qual)
  
  print("quantile_0.95 is / 95 % Upper Credible Interval is")
  posterior_quantile_0.95_Qual = qbeta(0.95, posterior_alpha_Qual,posterior_beta_Qual)
  print(posterior_quantile_0.95_Qual)
  
  print("posterior mode is (experiment if this actually conferges with the above")
  posterior_mode_Qual = qbeta(0.5, posterior_alpha_Qual,posterior_beta_Qual)
  print(posterior_mode_Qual)
  
  print("90% chance that the value is between") 
  print(posterior_quantile_0.05_Qual) 
  print("and") 
  print(posterior_quantile_0.95_Qual)
  
  
  print("***************************************************************************Hyper prior Update with Qualitative Results Credible Interval ended: ")
  
  posterior1_Qual = dbeta(Theta, posterior_alpha_Qual, posterior_beta_Qual)
  posterior1_normalised_Qual = posterior1_Qual/sum(posterior1_Qual)

  
  length(posterior1_Qual)
  length(Theta)
  
  plot(Theta, posterior1_Qual)
  
  
  
  
  print("plotting graph Posterior density distribution")
  density_posterior_Qual = data.frame(Theta, posterior1_Qual, mode_posterior_Qual, mean_posterior_Qual, posterior_quantile_0.05_Qual, posterior_quantile_0.95_Qual)
  
  
  graph_Posterior_Qual = plotDensity(data = density_posterior_Qual,
                                aes( x = density_posterior_Qual$Theta, 
                                     y = density_posterior_Qual$posterior1_Qual,
                                     fill = NULL),
                                mode = density_posterior_Qual$mode_posterior_Qual,
                                mean = density_posterior_Qual$mean_posterior_Qual, 
                                quantile_0.05 = density_posterior_Qual$posterior_quantile_0.05_Qual,
                                quantile_0.95 = density_posterior_Qual$posterior_quantile_0.95_Qual,
                                MAPhyperprior = 0.422856163424018,
                                CIUpperhyperprior = 0.640617186187267, 
                                CILowerhyperprior = 0.220057520487314, 
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
  
  print("****************************************** TOTAL N across quant studies ******************************************************************")
  
  
  print("Calculate total N for this Construct:")
  print(Construct)
  PooledN_output = PooledN(data = data, Construct = Construct)
  print("N is")
  N =PooledN_output$N
  print("total N for this construct across the included studies =")
  print(N)
  
  print("****************************************** Odds Ratio Pooled OR across Quant Studies ******************************************************")
  
  print("Odds Ratio for this Construct:")
  print(Construct)
  
  
  meta_data_likelihoodResults = metaDataLikelihood(likelihood_data = likelihood_data, Construct = Construct)
  LOGOdds_Ratio = meta_data_likelihoodResults$LOGOdds_Ratio
  
  print("log OR is")
  print(LOGOdds_Ratio)
  
  print("number of studies is")
  k =  meta_data_likelihoodResults$k

  LowerCI_LogOddsRatio = meta_data_likelihoodResults$LowerCI_LogOddsRatio 
  UpperCI_LogOddsRatio = meta_data_likelihoodResults$UpperCI_LogOddsRatio
  
  
  print("**************************************** LIKELIIHOOD ***************************************************************************************")
  
  #https://mc-stan.org/bayesplot/articles/plotting-mcmc-draws.html  plotting mcmc 
  
  set.seed(seed)
  tolerance = 1e-5
  nmc = 1e+9
  
  
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
  

  posterior_density = function(beta, 
                               N_PA_X, 
                               N_noPA_X, 
                               N_PA_noX,     
                               N_PA, 
                               N_X, 
                               N){
    
    
    probability_PA_X = expit(beta[2])
    
    Vector_of_PA_X = rbinom(n = N_PA, size = 1, prob = probability_PA_X)
    
    probability_PA_X_all = rlogis(n = N_PA_X, location = beta[1], scale =beta[2])
    probability_PA_X_distribution  = plogis(q =probability_PA_X_all, location = beta[1], scale = beta[2], log.p = TRUE)
    probability_PA_X_quantile = qlogis(probability_PA_X_distribution, location = beta[1], scale = beta[2], log.p = TRUE)
    
    Likelihood_PA_X = sum(dbinom(x = Vector_of_PA_X, size = 1, prob = probability_PA_X, log = TRUE))
  
    #MAKE THE BELOW UNCERTAINTY AS AN ARGUMENT TO A FUNCTION AND SOURCES THE CODE USING DIFFERENT SEEDS (x10) -- was done 
    PA_prior = sum(dnorm(beta, 0, uncertainty, log = TRUE))

    Posterior = Likelihood_PA_X + PA_prior
    return(params = list(probability_PA_X = probability_PA_X, Likelihood_PA_X = Likelihood_PA_X, beta = beta, Posterior = Posterior))
  }
  
  
  #I put the proposal calculations separetly so I can get mcmc object out as a histagram (while also having the results) 
  #Below I estimate the probability density and distirbution of PA|X from the contigency table. The probabilitydistirbutions are plotted using the Markov chain Monte Carlo with Gibbs sampling. 
#the results are  based on 50 000 iterations produced after the initial period of another 50 000 iterations. This was sufficient to achieve convergence. Convergence/Acceptance diagnostics were performed on the 50 000 monitored iterations.
  # The convergence/acceptance was monitored using  the Brooks–Gelman–Rubin statistic (Brooks and Gelman, 1998) with three chains starting from widely dispersed initial values. 
  #For Markov chain Monte Carlo with a single chain, convergence/acceptance was assessed graphically with trace plots. 
  
  run_metropolis_MCMC = function(startingValues, iterations){
    samples = array(dim = c(iterations+1,2), dimnames = NULL)
    samples[1,] = startingValues
    
    
    
    for (i in 1:iterations){
     
      
      
      proposal = samples[i,] +  rnorm(n = 2, mean = 0, sd = 0.1)
      
      new_parameterList_likelihood = posterior_density(beta = proposal, 
                                         N_PA_X = N_PA_X, 
                                         N_noPA_X = N_noPA_X, 
                                         N_PA_noX = N_PA_noX, 
                                         N_PA = N_PA, 
                                         N_X =  N_X, 
                                         N = N) 
    
      
      old_parameterList_likelihood = posterior_density(beta = samples[i,], 
                                         N_PA_X = N_PA_X, 
                                         N_noPA_X = N_noPA_X,
                                         N_PA_noX = N_PA_noX, 
                                         N_PA = N_PA, 
                                         N_X =  N_X, 
                                         N = N)
      

      
      
      likelihood_ratio = exp(new_parameterList_likelihood$Posterior - old_parameterList_likelihood$Posterior) #convergence criterion 
 
      
      U <- runif(1)  
      if (U < likelihood_ratio){
        
        samples[i+1,] = proposal
        
   
        
        alpha = proposal[1]
        probability_PA_X_expit = expit(proposal[2])
        scale = proposal[2]

        
        
      }else{
        samples[i+1,] = samples[i,]
      }
    }
    return(samples)} 
  
  
  print(" Start calculating Probability_PA_X")
  
  startingValues = c(0, 0.5)
  samples =  run_metropolis_MCMC(startingValues = startingValues, iterations = 100000)
  burnIn = 50000
  
  acceptance = 1-mean(duplicated(samples[-(1:burnIn),]))
  
  
  print("acceptance is:")
  print(acceptance)
  
  hist(samples[-(1:burnIn),1],nclass=30, main="Posterior of alpha, scale", xlab="alpha" )
  
  hist(samples[-(1:burnIn),2],nclass=30, main="Posterior of beta, location", xlab="beta" )
  
  print("samples are below")
  print(samples)
  
  a = mean(samples[-(1:burnIn),1])
  
  b = mean(samples[-(1:burnIn),2]) 
  
  print("the mean alpha is below")
  print(a)
  print("the mean beta is below")
  print(b)
  
  # the mcmc object above is an array and I only need the last 100 values which are values it converged on in the convernage MH sampling. Samples is an array of (ittirations, 2) 2 are alpha and beta 
  #taking the last 100 values for beta, and 100 values for alpha and taking a mean of it is going to be an accurate estimate of alpha and beta. 
  print("the samples 1 are below")
  alpha_and_beta = tail(samples[-(1:burnIn),1], n=100)
  
  
  alpha = mean(alpha_and_beta[1])
  print(alpha)
  
  
  print("the samples 2 are below")
  beta = mean(alpha_and_beta[2])
  print(beta)
  
  
  
  
  print(sprintf("the total N is: %s, N_PA: %s ||| N_X: %s ||| N_PA_X: %s  ||| N_PAnoX:  %s, N_noPA_X: %s", N, N_PA, N_X ,N_PA_X, N_PA_noX, N_noPA_X))
  
  probability_PA_X_expit = expit(b)
  
  #Let’s say you are working with probabilities very close to 1. 
  #Then the appropriate representation is not logp but log(1−p). To convert a probability from a log-complement scale to the logit scale, use lower.tail = FALSE:
  
  
  probability_PA_X_all = rlogis(n = N_PA_X, location = a, scale = b)
  
  Theta = seq(0, 1, 1/N_PA_X)
  
  print(Theta)
  
  probability_PA_X_distribution  = plogis(q = Theta, location = a, scale = b,lower.tail = FALSE, log.p = TRUE)
  MeanDistribution = mean(probability_PA_X_distribution)
  MeanDistribution_value = MeanDistribution
  MeanDistribution = rep(MeanDistribution, times = N_PA_X+1)
  
  probability_PA_X_quantile = qlogis(p = probability_PA_X_distribution, location =a, lower.tail = FALSE, scale = b, log.p = TRUE)
  
  probability_PA_X_density = dlogis(x = Theta, location = a, scale = b, log = TRUE)
  probability_PA_X_density_normalised = probability_PA_X_density/sum(probability_PA_X_density)
  
  print("the rlogis is below")
  print(probability_PA_X_all)
  

  probability_PA_X_density_expit = expit(probability_PA_X_density)
  probability_PA_X_Density_fromDATA_normalised = probability_PA_X_density_expit/sum(probability_PA_X_density_expit)
  
  
  print("the density::::::::::::::: is below")
  print(probability_PA_X_density)
  print(probability_PA_X_Density_fromDATA_normalised)
  
  
  
  MeanDensity = mean(probability_PA_X_density)
  MeanDensity_value = MeanDensity
  MeanDensity = rep(MeanDensity, times =  N_PA_X+1)

  
  first_Quantilie_Density = qlogis(0.05,  a, b)
  first_Quantilie_Density_value = first_Quantilie_Density
  first_Quantilie_Density = rep(first_Quantilie_Density, times =  N_PA_X+1)
  
  second_Quantilie_Density = qlogis(0.95, a, b)
  second_Quantilie_Density_value = second_Quantilie_Density
  second_Quantilie_Density = rep(second_Quantilie_Density, times =  N_PA_X+1)
  
  ModeDensity = qlogis(0.5,  a, b)
  ModeDensity_value = ModeDensity
  ModeDensity = rep(ModeDensity, times =  N_PA_X+1)
  
  
  
  
  print(sprintf("theta %s, probability_PA_X_density %s, MeanDensity %s,  ModeDensity %s, first_Quantilie_Density %s, second_Quantilie_Density %s", length(Theta), length(probability_PA_X_density_normalised), length(MeanDensity), length(ModeDensity), length(first_Quantilie_Density), length(second_Quantilie_Density)))
  print(dim(Theta))
  print(dim(probability_PA_X_density_normalised))
  print(dim(MeanDensity))
  print(dim(ModeDensity))
  print(dim(first_Quantilie_Density))
  print(dim(second_Quantilie_Density))
  
  Prob_PA_X_DATA = data.frame(Theta, probability_PA_X_density, probability_PA_X_density_normalised, MeanDensity, ModeDensity, first_Quantilie_Density, second_Quantilie_Density)
  print(head(Prob_PA_X_DATA))
  
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
  print(head(Prob_PA_X_DATA))
  
  graph_Likelihood_Probability_PA_X_density = plotDensity(data = Prob_PA_X_DATA,
                                                          aes(x = Prob_PA_X_DATA$Theta, 
                                                              y = Prob_PA_X_DATA$probability_PA_X_density,
                                                              fill = Prob_PA_X_DATA$likelihood_CI),
                                                          mean = MeanDensity_value, 
                                                          mode = ModeDensity_value,
                                                          MAPhyperprior = 0.422856163424018,
                                                          CIUpperhyperprior = 0.640617186187267, 
                                                          CILowerhyperprior = 0.220057520487314, 
                                                          quantile_0.05 = first_Quantilie_Density_value,
                                                          quantile_0.95 = second_Quantilie_Density_value,
                                                          xlabTitle = paste(print(sprintf("Join probability density of physical activity and %s, pooled estimate from quantitiative studies.", Construct))), 
                                                          ylabTitle = "Data: Probability Density", 
                                                          
                                                          title = Construct)
  print(graph_Likelihood_Probability_PA_X_density)
  print("Density graph is printed")
  
  
  Prob_PA_X_DATA_distribution = data.frame(Theta, probability_PA_X_distribution, MeanDistribution, ModeDensity, ModeDensity_value, first_Quantilie_Density, second_Quantilie_Density)
  
  Prob_PA_X_DATA_distribution_cumsum = cumsum(probability_PA_X_distribution)
  Distribution_CI = ifelse(
    Prob_PA_X_DATA_distribution_cumsum<0.025|Prob_PA_X_DATA_distribution_cumsum>0.975,
    "outside CI","inside CI")
  
  
  
  Prob_PA_X_DATA_distribution = cbind(Prob_PA_X_DATA_distribution, Distribution_CI)
  print(head(Prob_PA_X_DATA_distribution))
  print(Prob_PA_X_DATA_distribution)
  
  #graph_Likelihood_Probability_PA_X_distribution = plotDensity(data = Prob_PA_X_DATA_distribution,
  #                                                        aes(x = Prob_PA_X_DATA_distribution$Theta, 
  #                                                            y = Prob_PA_X_DATA_distribution$probability_PA_X_distribution,
  #                                                            fill = NULL),
  #                                                        mean = MeanDistribution, 
  #                                                        mode = ModeDensity_value,
  #                                                        quantile_0.05 = first_Quantilie_Density_value,
  #                                                        quantile_0.95 = second_Quantilie_Density_value,
  #                                                        xlabTitle = paste(print(sprintf("Join probability density of physical activity and %s, pooled estimate from quantitiative studies.", Construct))), 
  #                                                        ylabTitle = "Data: Probability Density", 
  #                                                        
  #                                                        title = Construct)
  #print(graph_Likelihood_Probability_PA_X_distribution)
  
  #print("Distribution graph is printed")
  
  
  
  
  
  print("****************************************** N_PA_X: Number of people engaging in PA given Construct X **************************************")
  
  
  
  Likelihood_FromSampledBeta = data.frame(N_PA_X, 
                                          probability_PA_X_expit, 
                                          MeanDensity_value,
                                          a,
                                          b,
                                          N_noPA_noX, 
                                          N_noPA_X, 
                                          N_PA_noX) 
  print("the likelihoood results summary to be outputted as Likelihood_FromSampledBeta")
  print(Likelihood_FromSampledBeta)
  
  print("****************************************** POSTERIOR ****************************************************************************************")
  
  #the posterior below is produced using Bayes update as specified by Spigielhalter et al. (2003) for beta-bernoulli distribution update (p60-62)
  print(Construct)
  
  
  print("posterior alpha")
  posterior_alpha = posterior_alpha_Qual + N_PA_X
  print(posterior_alpha)
  
  print("posterior beta")
  posterior_beta = posterior_beta_Qual + N - N_PA_X
  print(posterior_beta)
  
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
  
  #an alternative way of deriving the posterior is below, however, it will not print the plot syaing the finite value for y is required. The below is incorrect and is not reported but check with Spighelhalter again
  
  posterior3 = dbeta(Theta, HyperPrior_a * LOGOdds_Ratio, HyperPrior_b * (1/LOGOdds_Ratio))
  posterior3_normalised = posterior3/sum(posterior3)
  
  length(posterior1)
  length(Theta)
  
  plot(Theta, posterior1)
  
  
  
  
  print("plotting graph Posterior density distribution")
  density_posterior = data.frame(Theta, posterior1, mode_posterior, mean_posterior, posterior_quantile_0.05, posterior_quantile_0.95)
  
  
  graph_Posterior = plotDensity(data = density_posterior,
                                aes( x = density_posterior$Theta, 
                                     y = density_posterior$posterior1,
                                     fill = NULL),
                                mode = density_posterior$mode_posterior,
                                mean = density_posterior$mean_posterior, 
                                quantile_0.05 = density_posterior$posterior_quantile_0.05,
                                quantile_0.95 = density_posterior$posterior_quantile_0.95,
                                MAPhyperprior = 0.422856163424018,
                                CIUpperhyperprior = 0.640617186187267, 
                                CILowerhyperprior = 0.220057520487314, 
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
  
  
  
  #graph_PosteriorProbability_distribution = plotDensity(data = PosteriorProbability_distribution,
   #                                                     aes( x = PosteriorProbability_distribution$Theta, 
    #                                                         y = PosteriorProbability_distribution$ProbabilityDistribution_Posterior,
     #                                                        fill = NULL),
      #                                                  mode = PosteriorProbability_distribution$mode_posterior,
       #                                                 mean = PosteriorProbability_distribution$mean_posterior, 
        #                                                quantile_0.05 = PosteriorProbability_distribution$posterior_quantile_0.05,
         #                                               quantile_0.95 = PosteriorProbability_distribution$posterior_quantile_0.95,
          #                                              xlabTitle = paste("Posterior probability of physical activity conditioned on", print(Construct)),  
           #                                             ylabTitle = "Probability distribution", 
            #                                            title = Construct)
  
  #print(graph_PosteriorProbability_distribution)
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
                              
                              posterior_alpha = posterior_alpha,
                              posterior_beta = posterior_beta,  
                              mode_posterior =  mode_posterior,
                              mean_posterior = mean_posterior, 
                              posterior_CredibleInterval_0.05 = posterior_quantile_0.05, 
                              posterior_CredibleInterval_0.95 = posterior_quantile_0.95)))
  
}



#function for computin prior, likelihood and posterior density. reference: https://rpubs.com/RRisto/betadist
#function for metropolis-hestings sampling, reference: https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/
