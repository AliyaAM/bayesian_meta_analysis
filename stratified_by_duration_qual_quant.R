

library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(filenamer)
library(reshape2)  
library(tibble)
library(compute.es)
library(metafor)
library(bayesplot)
library(ggplot2)
library(ggridges)
library(rstan) 
library(coda)
library(bayestestR)
library(HDInterval)
library(assertthat)
library(RColorBrewer)


# Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"


source(paste(SOURCE_ROOT, "BayesUpdateStepByStep.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 

source(paste(SOURCE_ROOT, "Summary_stats_table_qual_and_quant.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence, and outputs the summary stats only 




x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
ALL_data_extracted = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)

data = filter(ALL_data_extracted, ALL_data_extracted$PA_Varme == "Duration_dayMins")
PA_Varme = "Duration_dayMins"

x$Construct =c("Age",
               "SelfEfficacy",
               "SocialSupport",
               "Comorbidity",
               "NegativeAttitude",
               "6MWT",
               "PhysicalFunctioning",
               "Symptoms",
               "LVEF5",
               "PositiveAttitude") 

unique(data$Construct)
Results_Duration_dayMins_qual_quant = data.frame()
unique(data$PA_Varme)





source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep="")) #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 
likelihood_data =  ConvertEffectsizes(data = data)



#social support, 
#Age, 6MWT,LVEF,Comorbidity1,
#symptoms, selfefficacy, negative attitude, positive attitude, physical functioning 


Duration_dayMins_Age = BayesUpdateStepByStep(x = x, Construct = "Age")
Results_Duration_dayMins_qual_quant = rbind(Results_Duration_dayMins_qual_quant, Duration_dayMins_Age)
Duration_dayMins_6MWT = BayesUpdateStepByStep(x = x, Construct = "6MWT")
Results_Duration_dayMins_qual_quant = rbind(Results_Duration_dayMins_qual_quant, Duration_dayMins_6MWT)

Duration_dayMins_Comorbidity1 = BayesUpdateStepByStep(x = x, Construct = "Comorbidity")
Results_Duration_dayMins_qual_quant = rbind(Results_Duration_dayMins_qual_quant, Duration_dayMins_Comorbidity1)


Duration_dayMins_Symptoms = BayesUpdateStepByStep(x = x, Construct = "Symptoms")
Results_Duration_dayMins_qual_quant = rbind(Results_Duration_dayMins_qual_quant, Duration_dayMins_Symptoms)

Duration_dayMins_SelfEfficacy = BayesUpdateStepByStep(x = x, Construct = "SelfEfficacy")
Results_Duration_dayMins_qual_quant = rbind(Results_Duration_dayMins_qual_quant, Duration_dayMins_SelfEfficacy)


Duration_dayMins_PhysicalFunctioning7 = BayesUpdateStepByStep(x = x, Construct = "PhysicalFunctioning")
Results_Duration_dayMins_qual_quant = rbind(Results_Duration_dayMins_qual_quant, Duration_dayMins_PhysicalFunctioning7)




Duration_dayMins_NegativeAttitude2 = BayesUpdateStepByStep(x = x, Construct = "NegativeAttitude")
Results_Duration_dayMins_qual_quant = rbind(Results_Duration_dayMins_qual_quant, Duration_dayMins_NegativeAttitude2)



Duration_dayMins_PositiveAttitude2 = BayesUpdateStepByStep(x = x, Construct = "PositiveAttitude")
Results_Duration_dayMins_qual_quant = rbind(Results_Duration_dayMins_qual_quant, Duration_dayMins_PositiveAttitude2)






Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = data.frame()
Summary_stats_table_qual_and_quantDuration_dayMins_Age = Summary_stats_table_qual_and_quant(x = x, Construct = "Age")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, Summary_stats_table_qual_and_quantDuration_dayMins_Age)


Summary_stats_table_qual_and_quantDuration_dayMins_Comorbidity1 = Summary_stats_table_qual_and_quant(x = x, Construct = "Comorbidity")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, Summary_stats_table_qual_and_quantDuration_dayMins_Comorbidity1)

Summary_stats_table_qual_and_quantDuration_dayMins_6MWT = Summary_stats_table_qual_and_quant(x = x, Construct = "6MWT")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, Summary_stats_table_qual_and_quantDuration_dayMins_6MWT)

Summary_stats_table_qual_and_quantDuration_dayMins_PhysicalFunctioning7  = Summary_stats_table_qual_and_quant(x = x, Construct = "PhysicalFunctioning")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, Summary_stats_table_qual_and_quantDuration_dayMins_PhysicalFunctioning7)


Summary_stats_table_qual_and_quantDuration_dayMins_Symptoms  = Summary_stats_table_qual_and_quant(x = x, Construct = "Symptoms")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, Summary_stats_table_qual_and_quantDuration_dayMins_Symptoms)

Summary_stats_table_qual_and_quantDuration_dayMins_SelfEfficacy  = Summary_stats_table_qual_and_quant(x = x, Construct = "SelfEfficacy")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, Summary_stats_table_qual_and_quantDuration_dayMins_SelfEfficacy)



Summary_stats_table_qual_and_quantDuration_dayMins_NegativeAttitude2  = Summary_stats_table_qual_and_quant(x = x, Construct = "NegativeAttitude")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, Summary_stats_table_qual_and_quantDuration_dayMins_NegativeAttitude2)


Summary_stats_table_qual_and_quantDuration_dayMins_PositiveAttitude2  = Summary_stats_table_qual_and_quant(x = x, Construct = "PositiveAttitude")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = rbind(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, Summary_stats_table_qual_and_quantDuration_dayMins_PositiveAttitude2)


write.table(Results_Duration_dayMins_qual_quant, file = paste(OUTPUT_ROOT, "Results_Duration_dayMins_qual_quant.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, file = paste(OUTPUT_ROOT, "Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )

Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$QualplusQuantSD = sqrt(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$posterior_QualplusQuant_variance)


Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant %>% 
  mutate_if(is.numeric, round, digits = 2)


Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$prior_CI = paste("[", Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Prior_qual_quantile_0.05, ";", Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Prior_qual_quantile_0.95, "]", sep = "")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Prior_qual_quantile_0.05 = Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$prior_CI


Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Likelihood_CI = paste("[", Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Likelihood_qual_quantile_0.05, ";", Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Likelihood_qual_quantile_0.95, "]", sep = "")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Likelihood_qual_quantile_0.05 = Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Likelihood_CI


Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Posterior_CI = paste("[", Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Posterior_QualplusQuant_quantile_0.05, ";", Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Posterior_QualplusQuant_quantile_0.95, "]", sep = "")
Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Posterior_QualplusQuant_quantile_0.05 = Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Posterior_CI

Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant = data.frame(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Construct,
                                                                                     Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Prior_qual_quantile_0.50,
                                                                                     Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Prior_qual_quantile_0.05,
                                                                                     Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Likelihood_qual_quantile_0.50,
                                                                                     Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Likelihood_qual_quantile_0.05,
                                                                                     Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Posterior_QualplusQuant_quantile_0.50,	
                                                                                     Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$Posterior_QualplusQuant_quantile_0.05, 
                                                                                     Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant$QualplusQuantSD) 

colnames(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant) = c("Construct",
                                                                                      "Expected value (log OR)", 
                                                                                      "95% CrI",
                                                                                      "Expected value (log OR)", 
                                                                                      "95% CrI", 
                                                                                      "Expected value (log OR)", 
                                                                                      "95% CrI", 
                                                                                      "SD")


folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}


write.table(Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant, file = paste(folder, "_edited_Summary_stats_table_qual_and_quantResults_Duration_dayMins_qual_quant.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



density_by_Construct_stratified = function(data, Construct){
  index = data$Construct == Construct
  logOddsRatio = seq(-3, 4  , length=1000)
  filtered_data = filter(data, Construct == data[index,]$Construct)
  
  # the results of the expert elicitation task 
  Prior_qual_density = dnorm(logOddsRatio,
                             filtered_data$logOR_expert_elicitation_task, 
                             filtered_data$variance_expert_elicitation_task)
  
  # posterior resulted from updating hyperprior with the results of the expert elicitaiton task 
  Posterior_qual_only = dnorm(logOddsRatio, 
                              filtered_data$Posterior_qual_only_mean,  
                              filtered_data$Posterior_qual_only_variance)
  
  # likelihood (quantitative evidence only)
  Likelihood = dnorm(logOddsRatio, 
                     filtered_data$LOGOdds_Ratio_quant, 
                     filtered_data$variance_quant)
  
  
  # the posterior resulted from updating prior with likelihood 
  posterior_QualplusQuant = dnorm(logOddsRatio, 
                                  filtered_data$posterior_QualplusQuant_mean,
                                  filtered_data$posterior_QualplusQuant_variance)
  
  # the posterior resulted from updating hyperprior with prior and then with likelihood 
  posterior_All = dnorm(logOddsRatio, 
                        filtered_data$posterior_All_mean,
                        filtered_data$posterior_All_variance)
  
  df = data.frame(logOddsRatio, Construct, Prior_qual_density, Posterior_qual_only, Likelihood, posterior_QualplusQuant, posterior_All)
  colnames(df) = c("logOddsRatio", "Construct", "Prior_qual_density", "Posterior_qual_only", "Likelihood",  "posterior_QualplusQuant", "posterior_All")
  return(df)
}
data = Results_Duration_dayMins_qual_quant



Age_density_by_Construct_stratified = density_by_Construct_stratified(data = Results_Duration_dayMins_qual_quant, Construct = "Age")
SixMWT_density_by_Construct_stratified = density_by_Construct_stratified(data = Results_Duration_dayMins_qual_quant, Construct = "6MWT")
Comorbidity1_density_by_Construct_stratified = density_by_Construct_stratified(data = Results_Duration_dayMins_qual_quant, Construct = "Comorbidity")

Symptoms_density_by_Construct_stratified = density_by_Construct_stratified(data = Results_Duration_dayMins_qual_quant, Construct = "Symptoms")

SelfEfficacy_density_by_Construct_stratified = density_by_Construct_stratified(data = Results_Duration_dayMins_qual_quant, Construct = "SelfEfficacy")
NegativeAttitude2_density_by_Construct_stratified = density_by_Construct_stratified(data = Results_Duration_dayMins_qual_quant, Construct = "NegativeAttitude")
PositiveAttitude2_density_by_Construct_stratified = density_by_Construct_stratified(data = Results_Duration_dayMins_qual_quant, Construct = "PositiveAttitude")

PhysicalFunctioning7_density_by_Construct_stratified = density_by_Construct_stratified(data = Results_Duration_dayMins_qual_quant, Construct = "PhysicalFunctioning")



height = c(rep(1, 1000),
           rep(2, 1000), 
           rep(3, 1000), 
           rep(4, 1000), 
           rep(5, 1000), 
           rep(6, 1000), 
           rep(7, 1000), 
           rep(8, 1000))

length(height)
density_ALL_Construct_quant_stratified = rbind(Age_density_by_Construct_stratified,
                                               SixMWT_density_by_Construct_stratified, 
                                               Comorbidity1_density_by_Construct_stratified,
                                               Symptoms_density_by_Construct_stratified,
                                               SelfEfficacy_density_by_Construct_stratified,
                                               PhysicalFunctioning7_density_by_Construct_stratified, 
                                               
                                               NegativeAttitude2_density_by_Construct_stratified,
                                               PositiveAttitude2_density_by_Construct_stratified)

density_ALL_Construct_quant_stratified = cbind(density_ALL_Construct_quant_stratified, height)



#plot4 = ggplot(density_ALL_Construct_quant_stratified, aes(x = Theta, y = construct, height=posterior, group = construct)) +
#  geom_density_ridges(stat = "identity", scale = 10) +
#  xlim(0, 0.5)
#plotting the results of the expert elicitation task 
Plot_Prior_qual_density = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Prior_qual_density, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3, 4  )

print(Plot_Prior_qual_density)

#plotting likelihood (quantitative evidence only)
Plot_Likelihood_stratified = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3, 4  )+ 
  
  scale_y_discrete(labels=c("6MWT"  =     "6MWT" ,
                            "Age"       =      "Age",
                            "Comorbidity"   =  "Comorbidity",
                            "PhysicalFunctioning" = "Physical Functioning", 
                            "NegativeAttitude" = "Negative Attitude", 
                            "PositiveAttitude"  = "Positive Attitude", 
                            "SelfEfficacy"    =     "Self-efficacy", 
                            "Symptoms" =   "Symptoms"))   
print(Plot_Likelihood_stratified)


#plotting the posterior resulted from updating prior with likelihood 
Plot_posterior_QualplusQuant = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=posterior_QualplusQuant, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3, 4  ) + 
  scale_y_discrete(labels=c("6MWT"  =     "6MWT" ,
                            "Age"       =      "Age",
                            "Comorbidity"   =  "Comorbidity",
                            "PhysicalFunctioning" = "Physical Functioning", 
                            "NegativeAttitude" = "Negative Attitude", 
                            "PositiveAttitude"  = "Positive Attitude", 
                            "SelfEfficacy"    =     "Self-efficacy", 
                            "Symptoms" =   "Symptoms"))     


print(Plot_posterior_QualplusQuant)



###################




density_ALL_Construct_quant_stratified
head(density_ALL_Construct_quant_stratified)


All_constructs_prior = select(density_ALL_Construct_quant_stratified, logOddsRatio, Construct, Prior_qual_density) 

All_constructs_likelihood = select(density_ALL_Construct_quant_stratified, logOddsRatio, Construct, Likelihood)

All_constructs_posterior = select(density_ALL_Construct_quant_stratified, logOddsRatio, Construct, posterior_QualplusQuant) 


Age_density_prior = All_constructs_prior  %>% filter(Construct == 'Age')
unique(Age_density_prior$Construct)



Comorbidity_density_prior =  All_constructs_prior  %>% filter(Construct == 'Comorbidity')
NegativeAttitude_density_prior = All_constructs_prior  %>% filter(Construct == "NegativeAttitude")
PositiveAttitude_density_prior = All_constructs_prior  %>% filter(Construct == "PositiveAttitude")
SixMWT_density_prior = All_constructs_prior  %>% filter(Construct == "6MWT")
PhysicalFunctioning_density_prior = All_constructs_prior  %>% filter(Construct == "PhysicalFunctioning")
Symptoms_density_prior = All_constructs_prior  %>% filter(Construct== "Symptoms")
SelfEfficacy_density_prior = All_constructs_prior  %>% filter(Construct == "SelfEfficacy")



Age_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "Age")
Comorbidity_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "Comorbidity")
NegativeAttitude_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "NegativeAttitude")
PositiveAttitude_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "PositiveAttitude")
SixMWT_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "6MWT")
PhysicalFunctioning_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "PhysicalFunctioning")
Symptoms_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "Symptoms")
SelfEfficacy_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "SelfEfficacy")


Age_density_posterior = All_constructs_posterior  %>% filter(Construct  == "Age")
Comorbidity_density_posterior = All_constructs_posterior  %>% filter(Construct  == "Comorbidity")
NegativeAttitude_density_posterior = All_constructs_posterior  %>% filter(Construct  == "NegativeAttitude")
PositiveAttitude_density_posterior = All_constructs_posterior  %>% filter(Construct  == "PositiveAttitude")
SixMWT_density_posterior = All_constructs_posterior  %>% filter(Construct == "6MWT")
PhysicalFunctioning_density_posterior = All_constructs_posterior  %>% filter(Construct  == "PhysicalFunctioning")
Symptoms_density_posterior =All_constructs_posterior  %>% filter(Construct  == "Symptoms")
SelfEfficacy_density_posterior = All_constructs_posterior  %>% filter(Construct  == "SelfEfficacy")


prior_name = rep("Qualitative evidence", times = 1000)

likelihood_name = rep("Quantitative evidence", times = 1000)

posterior_name = rep("Posterior (Qual + QUANT)", times = 1000)


distribution = c(prior_name, likelihood_name, posterior_name)


height = c(rep(10, 1000),
           rep(20, 1000), 
           rep(30, 1000), 
           rep(40, 1000), 
           rep(50, 1000), 
           rep(60, 1000), 
           rep(70, 1000), 
           rep(80, 1000), 
           rep(90, 1000),
           rep(100, 1000), 
           rep(110, 1000),
           rep(120, 1000), 
           rep(130, 1000), 
           rep(140, 1000), 
           rep(150, 1000), 
           rep(160, 1000), 
           rep(170, 1000), 
           rep(180, 1000), 
           rep(190, 1000),
           rep(200, 1000), 
           rep(210, 1000),
           rep(220, 1000), 
           rep(230, 1000), 
           rep(240, 1000))




d <- data.frame(
  logOddsRatio = density_ALL_Construct_quant_stratified$logOddsRatio, 
  Construct = c(Age_density_prior$Construct,
                Age_density_likelihood$Construct,
                Age_density_posterior$Construct,
                
                Comorbidity_density_prior$Construct,
                Comorbidity_density_likelihood$Construct,
                Comorbidity_density_posterior$Construct,
                
                
                NegativeAttitude_density_prior$Construct,
                NegativeAttitude_density_likelihood$Construct,
                NegativeAttitude_density_posterior$Construct, 
                
                PositiveAttitude_density_prior$Construct,
                PositiveAttitude_density_likelihood$Construct,
                PositiveAttitude_density_posterior$Construct,
                
                
                SixMWT_density_prior$Construct,
                SixMWT_density_likelihood$Construct,
                SixMWT_density_posterior$Construct,
                
                PhysicalFunctioning_density_prior$Construct,
                PhysicalFunctioning_density_likelihood$Construct,
                PhysicalFunctioning_density_posterior$Construct,
                
                Symptoms_density_prior$Construct,
                Symptoms_density_likelihood$Construct,
                Symptoms_density_posterior$Construct,
                
                
                SelfEfficacy_density_prior$Construct,
                SelfEfficacy_density_likelihood$Construct,
                SelfEfficacy_density_posterior$Construct),
  
  y = c(Age_density_prior$Prior_qual_density,
        Age_density_likelihood$Likelihood,
        Age_density_posterior$posterior_QualplusQuant,
        
        Comorbidity_density_prior$Prior_qual_density,
        Comorbidity_density_likelihood$Likelihood,
        Comorbidity_density_posterior$posterior_QualplusQuant,
        
        
        NegativeAttitude_density_prior$Prior_qual_density,
        NegativeAttitude_density_likelihood$Likelihood,
        NegativeAttitude_density_posterior$posterior_QualplusQuant, 
        
        PositiveAttitude_density_prior$Prior_qual_density, 
        
        PositiveAttitude_density_likelihood$Likelihood,
        
        PositiveAttitude_density_posterior$posterior_QualplusQuant,
        
        
        SixMWT_density_prior$Prior_qual_density,
        SixMWT_density_likelihood$Likelihood,
        SixMWT_density_posterior$posterior_QualplusQuant,
        
        PhysicalFunctioning_density_prior$Prior_qual_density,
        PhysicalFunctioning_density_likelihood$Likelihood,
        PhysicalFunctioning_density_posterior$posterior_QualplusQuant,
        
        Symptoms_density_prior$Prior_qual_density,
        Symptoms_density_likelihood$Likelihood,
        Symptoms_density_posterior$posterior_QualplusQuant,
        
        
        SelfEfficacy_density_prior$Prior_qual_density,
        SelfEfficacy_density_likelihood$Likelihood,
        SelfEfficacy_density_posterior$posterior_QualplusQuant),
  
  distribution = distribution, 
  
  height = height)

d$group_name = paste0(as.character(d$Construct)," ", as.character(d$distribution))
# colors for the intermediate plots were as follows: 
#prior: "#CC79A7"
#posterior: "#D55E00"
# likelihood: "#009E73"

#we want the distributions to be same color as they are in the intermediate plots so prior should be "#CC79A7" and so on. 
# however, they do not look nice together so I will pick the same colors but more pastel, Set2 is nice: 
#display.brewer.all(colorblindFriendly = TRUE)
#colors = display.brewer.pal(c(1, 2, 4), "Set2")
#print the codes for the colors: 
#brewer.pal(n = 5, name = "Set2")

##############

#colors from the set2: "#66C2A5" "#FC8D62" "#E78AC3"
Compare_distributions_plot = ggplot(d, aes(x = logOddsRatio, 
                                           y = Construct,
                                           height = y, 
                                           group = group_name, 
                                           color = distribution,
                                           fill = distribution)) +
  
  geom_density_ridges(stat = "identity",
                      scale = 1) +
  
  #scale_fill_brewer(palette = "Set2")+
  #scale_color_brewer(palette = "Set2")+
  scale_fill_manual(values = c("#FC8D62" , "#E78AC3" ,"#66C2A5"))+
  scale_color_manual(values = c("#FC8D62" , "#E78AC3" ,"#66C2A5"))+
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      panel.grid.major = element_line(colour = "grey", size = 0.2),
      panel.grid.minor = element_line(colour = "grey", size = 0.1))+ 
  xlim(-3,3) +
  scale_y_discrete(labels=c("6MWT"  =     "6MWT" ,
                            "Age"       =      "Age",
                            "Comorbidity"   =  "Comorbidity",
                            "PhysicalFunctioning" = "Physical Functioning", 
                            "NegativeAttitude" = "Negative Attitude", 
                            "PositiveAttitude"  = "Positive Attitude", 
                            "SelfEfficacy"    =     "Self-efficacy", 
                            "Symptoms" =   "Symptoms"))    +
  
  theme(text = element_text(size = 25))   



folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}

ggsave(file = paste(folder, "/Compare_distributions_plot_duration.pdf",  sep=""),Compare_distributions_plot, width=4, height=3, units="in", scale=3)

