
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

## Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"


source(paste(SOURCE_ROOT, "BayesUpdate_Quant.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence


source(paste(SOURCE_ROOT, "Summary_stats_table.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence and outputs the summary stats only as opposed to the entire distribution 

## THE BAYES UPDA

x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
ALL_extracted_data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)

data = ALL_extracted_data %>% filter(ALL_extracted_data$PA_Varme == "Exercise_complient_Binary")


source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep="")) #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 
likelihood_data =  ConvertEffectsizes(data = data)



unique(data$Construct)
Results_Exercise_complient_Binary = data.frame()
unique(data$PA_Varme)


#Exercise_complient_Binary_Age = BayesUpdate_Quant(data = data, Construct = "Age")
#Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_Age)

Exercise_complient_Binary_6MWT = BayesUpdate_Quant(data = data, Construct = "6MWT")
Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_6MWT)



#Exercise_complient_Binary_BMI = BayesUpdate_Quant(data = data, Construct = "BMI")
#Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_BMI)

#Exercise_complient_Binary_LVEF = BayesUpdate_Quant(data = data, Construct = "LVEF")
#Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_LVEF)


Exercise_complient_Binary_Comorbidity = BayesUpdate_Quant(data = data, Construct = "Comorbidity")
Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_Comorbidity)

Exercise_complient_Binary_PeakVO2 = BayesUpdate_Quant(data = data, Construct = "PeakVO2")
Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_PeakVO2)
Exercise_complient_Binary_Depression = BayesUpdate_Quant(data = data, Construct = "Depression")
Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_Depression)
#####
Exercise_complient_Binary_Dysphoria = BayesUpdate_Quant(data = data, Construct = "Dysphoria")
Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_Dysphoria)

Exercise_complient_Binary_HFDuration = BayesUpdate_Quant(data = data, Construct = "HFDuration")
Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_HFDuration)



Exercise_complient_Binary_Hostility = BayesUpdate_Quant(data = data, Construct = "Hostility")
Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_Hostility)


Exercise_complient_Binary_Partner = BayesUpdate_Quant(data = data, Construct = "Partner")
Results_Exercise_complient_Binary = rbind(Results_Exercise_complient_Binary, Exercise_complient_Binary_Partner)



Summary_stats_tableResults_Exercise_complient_Binary = data.frame()


#Summary_stats_tableExercise_complient_Binary_Age = Summary_stats_table(data = data, Construct = "Age")
#Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_Age)


#Summary_stats_tableExercise_complient_Binary_BMI = Summary_stats_table(data = data, Construct = "BMI")
#Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_BMI)

#Summary_stats_tableExercise_complient_Binary_LVEF = Summary_stats_table(data = data, Construct = "LVEF")
#Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_LVEF)

Summary_stats_tableExercise_complient_Binary_Comorbidity = Summary_stats_table(data = data, Construct = "Comorbidity")
Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_Comorbidity)

Summary_stats_tableExercise_complient_Binary_6MWT = Summary_stats_table(data = data, Construct = "6MWT")
Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_6MWT)

Summary_stats_tableExercise_complient_Binary_PeakVO2 = Summary_stats_table(data = data, Construct = "PeakVO2")
Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_PeakVO2)


Summary_stats_tableExercise_complient_Binary_Depression = Summary_stats_table(data = data, Construct = "Depression")
Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_Depression)


Summary_stats_tableExercise_complient_Binary_Dysphoria = Summary_stats_table(data = data, Construct = "Dysphoria")
Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_Dysphoria)


Summary_stats_tableExercise_complient_Binary_HFDuration = Summary_stats_table(data = data, Construct = "HFDuration")
Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_HFDuration)



Summary_stats_tableExercise_complient_Binary_Hostility = Summary_stats_table(data = data, Construct = "Hostility")
Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_Hostility)



#Summary_stats_tableExercise_complient_Binary_Partner = Summary_stats_table(data = data, Construct = "Partner")
#Summary_stats_tableResults_Exercise_complient_Binary = rbind(Summary_stats_tableResults_Exercise_complient_Binary, Summary_stats_tableExercise_complient_Binary_Partner)




write.table(Results_Exercise_complient_Binary, file = paste(OUTPUT_ROOT, "Results_Exercise_complient_Binary.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_stats_tableResults_Exercise_complient_Binary, file = paste(OUTPUT_ROOT, "Summary_stats_tableResults_Exercise_complient_Binary.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )




########
Summary_stats_tableResults_Exercise_complient_Binary$QualplusQuantSD = sqrt(Summary_stats_tableResults_Exercise_complient_Binary$variance_quant)


Summary_stats_tableResults_Exercise_complient_Binary = Summary_stats_tableResults_Exercise_complient_Binary %>% 
  mutate_if(is.numeric, round, digits = 2)



Summary_stats_tableResults_Exercise_complient_Binary$Likelihood_CI = paste("[", Summary_stats_tableResults_Exercise_complient_Binary$Likelihood_qual_quantile_0.05, ";", Summary_stats_tableResults_Exercise_complient_Binary$Likelihood_qual_quantile_0.95, "]", sep = "")
Summary_stats_tableResults_Exercise_complient_Binary$Likelihood_qual_quantile_0.05 = Summary_stats_tableResults_Exercise_complient_Binary$Likelihood_CI


Summary_stats_tableResults_Exercise_complient_Binary = data.frame(Summary_stats_tableResults_Exercise_complient_Binary$Construct,
                                                           
                                                           Summary_stats_tableResults_Exercise_complient_Binary$Likelihood_qual_quantile_0.50,
                                                           Summary_stats_tableResults_Exercise_complient_Binary$Likelihood_qual_quantile_0.05,
                                                           
                                                           Summary_stats_tableResults_Exercise_complient_Binary$QualplusQuantSD) 

colnames(Summary_stats_tableResults_Exercise_complient_Binary) = c("Construct",
                                                            
                                                            "Expected value (log OR)", 
                                                            "95% CrI", 
                                                            "SD")




folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}



write.table(Summary_stats_tableResults_Exercise_complient_Binary, file = paste(folder, "_edited_Summary_stats_tableResults_Exercise_complient_Binary_QUANT.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
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

data = Results_Exercise_complient_Binary


#Age_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Age")

#LVEF_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "LVEF")

#BMI_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "BMI")

SixMWT_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "6MWT")
Comorbidity_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Comorbidity")

Depression_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Depression")

PeakVO2_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PeakVO2")

Dysphoria_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Dysphoria")


HFDuration_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "HFDuration")

Hostility_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Hostility")

#Partner_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Partner")


height = c(rep(1, 1000),
           rep(2, 1000),
           rep(3, 1000),
           rep(4, 1000),
           rep(5, 1000),
           rep(6, 1000),
           rep(7, 1000))

length(height)
density_ALL_Construct_quant_stratified = rbind(#Age_density_by_Construct_stratified, 
                                               #BMI_density_by_Construct_stratified, 
                                               SixMWT_density_by_Construct_stratified, 
                                              # LVEF_density_by_Construct_stratified, 
                                               Comorbidity_density_by_Construct_stratified, 
                                               Depression_density_by_Construct_stratified,
                                               HFDuration_density_by_Construct_stratified,
                                               Dysphoria_density_by_Construct_stratified,
                                               PeakVO2_density_by_Construct_stratified,
                                               #Partner_density_by_Construct_stratified,
                                               Hostility_density_by_Construct_stratified)

density_ALL_Construct_quant_stratified = cbind(density_ALL_Construct_quant_stratified, height)



#plot4 = ggplot(density_ALL_Construct_quant_stratified, aes(x = Theta, y = construct, height=posterior, group = construct)) +
#  geom_density_ridges(stat = "identity", scale = 10) +
#  xlim(0, 0.5)


#plotting likelihood (quantitative evidence only)
Plot_Likelihood_stratified = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3,3) +
  scale_y_discrete(labels=c("6MWT"  =     "6MWT" ,
                            "Comorbidity" = "Comorbidity",
                            "Hostility" = "Hostility",
                            "HFDuration" = "HF Duration", 
                            "Dysphoria" = "Dysphoria", 
                            "Depression" = "Depression"))   + 
  theme(text = element_text(size = 40))   


print(Plot_Likelihood_stratified)


folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}


ggsave(file = paste(folder, "Stratif_Plot_Likelihood_stratified_exercise_compl.pdf",  sep=""),Plot_Likelihood_stratified, width=4, height=3, units="in", scale=3)





