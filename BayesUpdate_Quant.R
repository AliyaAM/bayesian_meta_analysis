

# this function that runs Bayesian meta-analysis of quantitative evidence
## THE BAYES UPDATE WITHOUT THE PRIOR ELICITED FROM THE QUALITATIVE STUDIES
### Bayes update: Jaarsma's empirical hyperprior + quantitative studies 

#methods used here are identical to the methods used for the bayesian meta-analysis combining qualitative and quantitative studies, however here we omit the quanlitative evidence. 

library(ggplot2)
library(reshape2)  
library(tibble)
library(bayestestR)
library(HDInterval)





source(paste(SOURCE_ROOT, "PooledN.R", sep=""))  # calculate the total number of participants (N) across studies that evaluated each construct, read from the csv file QuantData

source(paste(SOURCE_ROOT, "PooledOddsRatio_metaanalysis.R", sep="")) #run the frequentisit meta-analysis (REML) pooling the findings from quantitative studies, stratified by construct. The Overall Effect estimate is (log) Odds Ratio, a list of pooled Log ORs, one per construct). 


# this function below produces  density, distribution and all summary statistic for: 
#############################################################  (a) the prior: the levels of physical activity worldwide (Jaarsma et al., 2013); 
#############################################################  (b) the likelihood (pooled estimates from teh quantitative studies);
#############################################################  (c) and posterior (the bayes update implimmmented per Spighelhalter et al., 2004 reccomendation).
#function for computin prior, likelihood and posterior density. reference: https://rpubs.com/RRisto/betadist

BayesUpdate_Quant <- function(data, Construct) {
  

  
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
  
  BayesUpdate_func = function(Construct, Total_N_hyperprior, Mean_probability_hyperprior, Variance_hyperprior, Log_Odds_hyperprior, 
                              LOGOdds_Ratio_quant, variance_quant) {
    

    Probability = seq( 0 , 1 , length=1000)
    logOddsRatio = seq( -1 , 2 , length=1000)

    #elicit HYPERPRIOR distribution as a Gaussian (aka normal) distribution with mean value = mean from Jaarsma, and variance from Jaarsma
    Variance_hyperprior = Variance_hyperprior
  

    #elicits hyperprior from arguments Total_N_hyperprior, Mean_probability_hyperprior, Variance_hyperprior, Probability
    Hyperprior_density = dnorm(Probability, Mean_probability_hyperprior,  Variance_hyperprior, log = FALSE)
    #normalised hyperprior distribution 
    Hyperprior_density = Hyperprior_density/sum(Hyperprior_density)
    
    data=data.frame(Probability,logOddsRatio,  Hyperprior_density, Mean_probability_hyperprior, Variance_hyperprior)
    data$Hyperprior_density_cumsum=cumsum(data$Hyperprior_density)
    data$Hyperprior_density_CI=ifelse(data$Hyperprior_density_cumsum<0.025|data$Hyperprior_density_cumsum>0.975, "outside CI", "inside CI")
    
    Log_Odds_hyperprior = Log_Odds_hyperprior
    data$Log_Odds_hyperprior = Log_Odds_hyperprior

    data$Hyperprior_logOdds = dnorm(logOddsRatio, data$Log_Odds_hyperprior,  data$Variance_hyperprior, log = FALSE)
    data$Hyperprior_logOdds = data$Hyperprior_logOdds/sum(data$Hyperprior_logOdds)
    data$Hyperprior_logOdds_cumsum = cumsum(data$Hyperprior_logOdds)
    data$Hyperprior_logOdds_CI = ifelse(data$Hyperprior_logOdds_cumsum<0.025|data$Hyperprior_logOdds_cumsum>0.975, "outside CI", "inside CI")
    
    #expected value of the log Odds Ratio according to the hyperprior: 
    summary_data = data.frame(Variance_hyperprior, Log_Odds_hyperprior)
    Hyperprior_quantile_0.50 = qnorm(0.50, Log_Odds_hyperprior, Variance_hyperprior, lower.tail = TRUE, log.p = FALSE)
    summary_data = cbind(summary_data, Hyperprior_quantile_0.50)
      

        
    #Credible Intervals: Hyperprior
    data$Hyperprior_quantile_0.05 = qnorm(0.05, data$Log_Odds_hyperprior, data$Variance_hyperprior, lower.tail = TRUE, log.p = FALSE)
    data$Hyperprior_quantile_0.95 = qnorm(0.95,  data$Log_Odds_hyperprior, data$Variance_hyperprior, lower.tail = TRUE, log.p = FALSE)
    
    
  
    
 
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
    
    #Formula from Spieghelhalter p 63: updating prior with likelihood using the following mean nd variance for the distribution: 
    data$posterior_Quant_mean = (data$Log_Odds_hyperprior/data$Variance_hyperprior + data$LOGOdds_Ratio_quant/data$variance_quant)/(1/data$Variance_hyperprior+1/data$variance_quant)
    data$posterior_Quant_variance =1/(1/data$Variance_hyperprior+1/data$variance_quant)
    #posterior distribution for updating prior with likelihood  
    data$Posterior_Quant = dnorm(logOddsRatio, data$posterior_Quant_mean,  data$posterior_Quant_variance, log = FALSE)
    #normalise posterior density distribution
    data$Posterior_Quant = data$Posterior_Quant/sum(data$Posterior_Quant)
    data$Posterior_Quant_cumsum=cumsum(data$Posterior_Quant)
    data$Posterior_Quant_CI=ifelse(data$Posterior_Quant_cumsum<0.025|data$Posterior_Quant_cumsum>0.975, "outside CI", "inside CI")
    data$p_Posterior_Quant = pnorm(logOddsRatio, data$posterior_Quant_mean, data$posterior_Quant_variance, lower.tail = TRUE, log.p = FALSE)
    #Credible Intervals: posterior (update without hyperprior)
    data$Posterior_Quant_quantile_0.05 = qnorm(0.05, data$posterior_Quant_mean, data$posterior_Quant_variance, lower.tail = TRUE, log.p = FALSE)
    data$Posterior_Quant_quantile_0.95 = qnorm(0.95,  data$posterior_Quant_mean, data$posterior_Quant_variance, lower.tail = TRUE, log.p = FALSE)
    
   
    data
    
  }
  
  data=BayesUpdate_func(Construct = Construct, 
                        Total_N_hyperprior = Total_N_hyperprior, 
                        Mean_probability_hyperprior = Mean_probability_hyperprior, 
                        Variance_hyperprior = Variance_hyperprior,
                        Log_Odds_hyperprior = Log_Odds_hyperprior, 
                        LOGOdds_Ratio_quant = LOGOdds_Ratio_quant, 
                        variance_quant = variance_quant)
  
  
  
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
  
  #plot hyperprior density distribution 
  plot_hyperprior_density = plotting(data=data,
                                     aes(x=Probability, y=Hyperprior_density, fill=Hyperprior_density_CI), 
                                     values_colour = c("#999999", "#0072B2"), 
                                     title="Hyperprior: Probability")
  
  #print plot, so we it can be saved into the local repository 
  print(plot_hyperprior_density)
  
  plot_hyperprior_density_Log_OR = plotting(data=data,
                                            aes(x=logOddsRatio, y=Hyperprior_logOdds, fill=Hyperprior_logOdds_CI), 
                                            values_colour = c("#999999", "#0072B2"), 
                                            title="Hyperprior: log Odds Ratio")
  
  print(plot_hyperprior_density_Log_OR)
  
  
  
  #plot likelihood 
  plot_Likelihood_density = plotting(data=data,
                                     aes(x=logOddsRatio, y=Likelihood, fill= Likelihood_CI), 
                                     values_colour = c("#009E73", "#0072B2"), 
                                     title = paste("Likelihood: Probability for physical activity given", print(Construct)))
  
  print(plot_Likelihood_density)
  
  #plot posterior (hyperprior updated with likelihood) 
  plot_Posterior_Quant = plotting(data=data,
                                     aes(x=logOddsRatio, y=Posterior_Quant, fill= Posterior_Quant_CI), 
                                     values_colour = c("#009E73", "#0072B2"), 
                                     title = paste("Posterior: Probability for physical activity given", print(Construct)))
  
  print(plot_Posterior_Quant)
  
  
  return(params = (data.frame(Construct = Construct, 
                              data)))
  
}




