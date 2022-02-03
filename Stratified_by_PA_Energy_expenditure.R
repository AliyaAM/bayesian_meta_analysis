



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


x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)
ALL_extracted_data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)


source(paste(SOURCE_ROOT, "BayesUpdate_Quant.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence
source(paste(SOURCE_ROOT, "Summary_stats_table.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence and outputs the summary stats only as opposed to the entire distribution 

data = ALL_extracted_data %>% filter(ALL_extracted_data$PA_Varme == "EnergyExpend_total")



unique(data$Construct)
Results_EnergyExpend_total = data.frame()
unique(data$PA_Varme)


source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep="")) #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 
likelihood_data =  ConvertEffectsizes(data = data)

EnergyExpend_totalSmoking = BayesUpdate_Quant(data = data, Construct = "Smoking")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_totalSmoking)


EnergyExpend_total_Income = BayesUpdate_Quant(data = data, Construct = "Income")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_Income)


EnergyExpend_total_Age = BayesUpdate_Quant(data = data, Construct = "Age")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_Age)



EnergyExpend_total_Ethnicity = BayesUpdate_Quant(data = data, Construct = "Ethnicity")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_Ethnicity)


EnergyExpend_total_LVAD = BayesUpdate_Quant(data = data, Construct = "LVAD")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_LVAD)


EnergyExpend_total_PhysicalFunctioning7 = BayesUpdate_Quant(data = data, Construct = "PhysicalFunctioning7")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_PhysicalFunctioning7)



EnergyExpend_total_BMI = BayesUpdate_Quant(data = data, Construct = "BMI")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_BMI)


EnergyExpend_total_QoL = BayesUpdate_Quant(data = data, Construct = "QoL")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_QoL)


EnergyExpend_total_HFrEF_Yes = BayesUpdate_Quant(data = data, Construct = "HFrEF_Yes")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_HFrEF_Yes)



EnergyExpend_total_SelfEfficacy = BayesUpdate_Quant(data = data, Construct = "SelfEfficacy")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_SelfEfficacy)


EnergyExpend_total_Employment = BayesUpdate_Quant(data = data, Construct = "Employment")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_Employment)



EnergyExpend_total_NegativeAttitude2 = BayesUpdate_Quant(data = data, Construct = "NegativeAttitude2")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_NegativeAttitude2)



EnergyExpend_total_PositiveAttitude2 = BayesUpdate_Quant(data = data, Construct = "PositiveAttitude2")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_PositiveAttitude2)


EnergyExpend_total_Symptoms = BayesUpdate_Quant(data = data, Construct = "Symptoms")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_Symptoms)



EnergyExpend_total_Symptoms_distress = BayesUpdate_Quant(data = data, Construct = "Symptoms_distress")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_Symptoms_distress)


EnergyExpend_total_6MWT = BayesUpdate_Quant(data = data, Construct = "6MWT")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_6MWT)

EnergyExpend_total_Comorbidity = BayesUpdate_Quant(data = data, Construct = "Comorbidity1")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_Comorbidity)


EnergyExpend_total_LVEF = BayesUpdate_Quant(data = data, Construct = "LVEF")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_LVEF)

EnergyExpend_total_Depression = BayesUpdate_Quant(data = data, Construct = "Depression2")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_Depression)
#####

EnergyExpend_total_Partner= BayesUpdate_Quant(data = data, Construct = "Partner")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_Partner)


EnergyExpend_total_HFDuration = BayesUpdate_Quant(data = data, Construct = "HFDuration")
Results_EnergyExpend_total = rbind(Results_EnergyExpend_total, EnergyExpend_total_HFDuration)





Summary_stats_tableResults_EnergyExpend_total = data.frame()



Summary_stats_tableEnergyExpend_total_Smoking = Summary_stats_table(data = data, Construct = "Smoking")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Smoking)


Summary_stats_tableEnergyExpend_total_Income = Summary_stats_table(data = data, Construct = "Income")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Income)




Summary_stats_tableEnergyExpend_total_Age = Summary_stats_table(data = data, Construct = "Age")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Age)


Summary_stats_tableEnergyExpend_total_Comorbidity = Summary_stats_table(data = data, Construct = "Comorbidity1")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Comorbidity)

Summary_stats_tableEnergyExpend_total_6MWT = Summary_stats_table(data = data, Construct = "6MWT")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_6MWT)

Summary_stats_tableEnergyExpend_total_LVEF = Summary_stats_table(data = data, Construct = "LVEF")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_LVEF)


Summary_stats_tableEnergyExpend_total_Depression = Summary_stats_table(data = data, Construct = "Depression2")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Depression)


Summary_stats_tableEnergyExpend_total_QoL = Summary_stats_table(data = data, Construct = "QoL")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_QoL)



Summary_stats_tableEnergyExpend_total_Partner = Summary_stats_table(data = data, Construct = "Partner")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Partner)



Summary_stats_tableEnergyExpend_total_HFDuration = Summary_stats_table(data = data, Construct = "HFDuration")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_HFDuration)


Summary_stats_tableEnergyExpend_total_Ethnicity  = Summary_stats_table(data = data, Construct = "Ethnicity")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Ethnicity)


Summary_stats_tableEnergyExpend_total_LVAD  = Summary_stats_table(data = data, Construct = "LVAD")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_LVAD)


Summary_stats_tableEnergyExpend_total_PhysicalFunctioning7  = Summary_stats_table(data = data, Construct = "PhysicalFunctioning7")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_PhysicalFunctioning7)


Summary_stats_tableEnergyExpend_total_BMI = Summary_stats_table(data = data, Construct = "BMI")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_BMI)



Summary_stats_tableEnergyExpend_total_HFrEF_Yes  = Summary_stats_table(data = data, Construct = "HFrEF_Yes")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_HFrEF_Yes)


Summary_stats_tableEnergyExpend_total_SelfEfficacy  = Summary_stats_table(data = data, Construct = "SelfEfficacy")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_SelfEfficacy)



Summary_stats_tableEnergyExpend_total_Employment  = Summary_stats_table(data = data, Construct = "Employment")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Employment)


Summary_stats_tableEnergyExpend_total_NegativeAttitude2  = Summary_stats_table(data = data, Construct = "NegativeAttitude2")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_NegativeAttitude2)


Summary_stats_tableEnergyExpend_total_PositiveAttitude2  = Summary_stats_table(data = data, Construct = "PositiveAttitude2")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_PositiveAttitude2)



Summary_stats_tableEnergyExpend_total_Symptoms  = Summary_stats_table(data = data, Construct = "Symptoms")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Symptoms)



Summary_stats_tableEnergyExpend_total_Symptoms_distress  = Summary_stats_table(data = data, Construct = "Symptoms_distress")
Summary_stats_tableResults_EnergyExpend_total = rbind(Summary_stats_tableResults_EnergyExpend_total, Summary_stats_tableEnergyExpend_total_Symptoms_distress)







write.table(Results_EnergyExpend_total, file = paste(OUTPUT_ROOT, "Results_EnergyExpend_total.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_stats_tableResults_EnergyExpend_total, file = paste(OUTPUT_ROOT, "Summary_stats_tableResults_EnergyExpend_total.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



########
Summary_stats_tableResults_EnergyExpend_total$QualplusQuantSD = sqrt(Summary_stats_tableResults_EnergyExpend_total$variance_quant)


Summary_stats_tableResults_EnergyExpend_total = Summary_stats_tableResults_EnergyExpend_total %>% 
  mutate_if(is.numeric, round, digits = 2)



Summary_stats_tableResults_EnergyExpend_total$Likelihood_CI = paste("[", Summary_stats_tableResults_EnergyExpend_total$Likelihood_qual_quantile_0.05, ";", Summary_stats_tableResults_EnergyExpend_total$Likelihood_qual_quantile_0.95, "]", sep = "")
Summary_stats_tableResults_EnergyExpend_total$Likelihood_qual_quantile_0.05 = Summary_stats_tableResults_EnergyExpend_total$Likelihood_CI


Summary_stats_tableResults_EnergyExpend_total = data.frame(Summary_stats_tableResults_EnergyExpend_total$Construct,
                                                         
                                                         Summary_stats_tableResults_EnergyExpend_total$Likelihood_qual_quantile_0.50,
                                                         Summary_stats_tableResults_EnergyExpend_total$Likelihood_qual_quantile_0.05,
                                                         
                                                         Summary_stats_tableResults_EnergyExpend_total$QualplusQuantSD) 

colnames(Summary_stats_tableResults_EnergyExpend_total) = c("Construct",
                                                          
                                                          "Expected value (log OR)", 
                                                          "95% CrI", 
                                                          "SD")




folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}



write.table(Summary_stats_tableResults_EnergyExpend_total, file = paste(folder, "_edited_Summary_stats_tableResults_EnergyExpend_total_QUANT.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )




density_by_Construct_stratified = function(data, Construct){
  index = data$Construct == Construct
  logOddsRatio = seq( -3, 4 , length=1000)
  filtered_data = filter(data, Construct == data[index,]$Construct)
  
  
  # likelihood (quantitative evidence only)
  Likelihood = dnorm(logOddsRatio, 
                     filtered_data$LOGOdds_Ratio_quant, 
                     filtered_data$variance_quant)
  
  # the posterior resulted from updating hyperprior with likelihood 
  posterior_Quant = dnorm(logOddsRatio, 
                          filtered_data$posterior_Quant_mean,
                          filtered_data$posterior_Quant_variance)
  
  
  df = data.frame(logOddsRatio, Construct, Likelihood, posterior_Quant)
  colnames(df) = c("logOddsRatio", "Construct", "Likelihood", "posterior_Quant")
  return(df)
}

data = Results_EnergyExpend_total


Income_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Income")

Smoking_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Smoking")


Age_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Age")
SixMWT_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "6MWT")

Comorbidity_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Comorbidity1")

LVEF_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "LVEF")
Depression_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Depression2")



Partner_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Partner")
HFDuration_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "HFDuration")

Ethnicity_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Ethnicity")
LVAD_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "LVAD")
PhysicalFunctioning7_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PhysicalFunctioning7")
BMI_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "BMI")
QoL_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "QoL")

HFrEF_Yes_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "HFrEF_Yes")
SelfEfficacy_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "SelfEfficacy")
Employment_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Employment")
NegativeAttitude2_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "NegativeAttitude2")
PositiveAttitude2_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PositiveAttitude2")
Symptoms_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Symptoms")
Symptoms_distress_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Symptoms_distress")






height = c(rep(1, 1000),
           rep(2, 1000), 
           rep(3, 1000), 
           rep(4, 1000), 
           rep(5, 1000), 
           rep(6, 1000), 
           rep(7, 1000), 
           rep(8, 1000), 
           rep(9, 1000),
           rep(10, 1000), 
           rep(11, 1000), 
           rep(12, 1000), 
           rep(13, 1000), 
           rep(14, 1000),
           rep(15, 1000), 
           rep(16, 1000), 
           rep(17, 1000), 
           rep(18, 1000), 
           rep(19, 1000), 
           rep(20, 1000), 
           rep(21, 1000))

length(height)
density_ALL_Construct_quant_stratified = rbind(Age_density_by_Construct_stratified,
                                               Income_density_by_Construct_stratified,
                                              
                                               Smoking_density_by_Construct_stratified,
                                               SixMWT_density_by_Construct_stratified, 
                                               Comorbidity_density_by_Construct_stratified,
                                               LVEF_density_by_Construct_stratified, 
                                               Depression_density_by_Construct_stratified,
                                               Partner_density_by_Construct_stratified,
                                               HFDuration_density_by_Construct_stratified, 
                                               Ethnicity_density_by_Construct_stratified, 
                                               LVAD_density_by_Construct_stratified, 
                                               PhysicalFunctioning7_density_by_Construct_stratified, 

                                               BMI_density_by_Construct_stratified, 
                                               HFrEF_Yes_density_by_Construct_stratified, 
                                               SelfEfficacy_density_by_Construct_stratified, 
                                               Employment_density_by_Construct_stratified, 
                                               NegativeAttitude2_density_by_Construct_stratified, 
                                               PositiveAttitude2_density_by_Construct_stratified, 
                                               Symptoms_density_by_Construct_stratified, 
                                               Symptoms_distress_density_by_Construct_stratified,
                                               QoL_density_by_Construct_stratified)

density_ALL_Construct_quant_stratified = cbind(density_ALL_Construct_quant_stratified, height)



#plot4 = ggplot(density_ALL_Construct_quant_stratified, aes(x = Theta, y = construct, height=posterior, group = construct)) +
#  geom_density_ridges(stat = "identity", scale = 10) +
#  xlim(0, 0.5)


#plotting likelihood (quantitative evidence only)
Plot_Likelihood_stratified_EE = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3,3) +
  
  # in the meta-analysis not stratified by the PA type, when a study included multiple PA outcomes we chose accelerometer units above energy expenditure and energy expenditure above self-report, 
  #self-report above a binary data, 
  #so when a study had multiple PA outcomes we tagged data as follows: the primary outcome for self-efficacy as that and the secondary outcome -- self-efficacy2, hense the names
  
  scale_y_discrete(labels=c("6MWT"  =     "6MWT" ,
                            "Age"       =      "Age",
                            "BMI"    =  "BMI",
                            "Comorbidity1"   =  "Comorbidity",
                            "Depression2"    =  "Depression",
                            "Employment"   =        "Employment",
                            "Ethnicity"   =   "Ethnicity", 
                            
                            "PhysicalFunctioning7" = "Physical Functioning", 
                            "HFrEF_Yes"     =   "HFrEF",    
                            "LVEF"    =         "LVEF",    
                                      
                            "NegativeAttitude2" = "Negative Attitude", 
                            "PositiveAttitude2"  = "Positive Attitude", 
                            "SelfEfficacy"    =     "Self-efficacy", 
                            "Symptoms" =   "Symptoms", 
                            "HFDuration"    =  "HF Duration",       
                            "Income"     =          "Income",   
                            "Smoking" =  "Smoking", 
                            
                            "Partner"   =  "Partner", 
                            "Symptoms_distress"    =  "Symptoms Distress" , 
                           "LVAD"   =  "LVAD",
                           "QoL" = "QoL"))   
                                    

print(Plot_Likelihood_stratified_EE)


folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}


ggsave(file = paste(folder, "Plot_Likelihood_stratified_EE.pdf",  sep=""),Plot_Likelihood_stratified_EE, width=4, height=3, units="in", scale=3)

