

# Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"



source(paste(SOURCE_ROOT, "BayesUpdate_Quant.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence


source(paste(SOURCE_ROOT, "Summary_stats_table.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence and outputs the summary stats only as opposed to the entire distribution 

## THE BAYES UPDA

x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
All_extracted_data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)

data = subset(All_extracted_data, All_extracted_data$PA_Varme == "Duration_dayMins")
PA_Varme = "Duration_dayMins"



unique(data$Construct)
Results_Exercise_complient_Binary_qual_quant = data.frame()
unique(data$PA_Varme)


Results_Duration_dayMins = data.frame()

source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep="")) #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 
likelihood_data =  ConvertEffectsizes(data = data)




Duration_dayMins_Age = BayesUpdate_Quant(data = data, Construct = "Age")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Age)



Duration_dayMins_Ethnicity = BayesUpdate_Quant(data = data, Construct = "Ethnicity")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Ethnicity)




Duration_dayMins_PhysicalFunctioning7 = BayesUpdate_Quant(data = data, Construct = "PhysicalFunctioning")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_PhysicalFunctioning7)



Duration_dayMins_BMI = BayesUpdate_Quant(data = data, Construct = "BMI")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_BMI)



Duration_dayMins_QoL = BayesUpdate_Quant(data = data, Construct = "QoL")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_QoL)


Duration_dayMins_HFrEF_Yes = BayesUpdate_Quant(data = data, Construct = "HFrEF_Yes2")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_HFrEF_Yes)



Duration_dayMins_SelfEfficacy = BayesUpdate_Quant(data = data, Construct = "SelfEfficacy")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_SelfEfficacy)


Duration_dayMins_Employment = BayesUpdate_Quant(data = data, Construct = "Employment2")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Employment)



Duration_dayMins_NegativeAttitude2 = BayesUpdate_Quant(data = data, Construct = "NegativeAttitude")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_NegativeAttitude2)



Duration_dayMins_PositiveAttitude2 = BayesUpdate_Quant(data = data, Construct = "PositiveAttitude")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_PositiveAttitude2)


Duration_dayMins_Symptoms = BayesUpdate_Quant(data = data, Construct = "Symptoms")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Symptoms)




Duration_dayMins_6MWT = BayesUpdate_Quant(data = data, Construct = "6MWT")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_6MWT)

Duration_dayMins_Comorbidity = BayesUpdate_Quant(data = data, Construct = "Comorbidity")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Comorbidity)


Duration_dayMins_Depression = BayesUpdate_Quant(data = data, Construct = "Depression")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Depression)
#####






Summary_stats_tableResults_Duration_dayMins = data.frame()





Summary_stats_tableDuration_dayMins_Age = Summary_stats_table(data = data, Construct = "Age")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Age)


Summary_stats_tableDuration_dayMins_Comorbidity = Summary_stats_table(data = data, Construct = "Comorbidity")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Comorbidity)

Summary_stats_tableDuration_dayMins_6MWT = Summary_stats_table(data = data, Construct = "6MWT")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_6MWT)


Summary_stats_tableDuration_dayMins_Depression = Summary_stats_table(data = data, Construct = "Depression")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Depression)




Summary_stats_tableDuration_dayMins_Ethnicity  = Summary_stats_table(data = data, Construct = "Ethnicity")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Ethnicity)


Summary_stats_tableDuration_dayMins_PhysicalFunctioning7  = Summary_stats_table(data = data, Construct = "PhysicalFunctioning")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_PhysicalFunctioning7)


Summary_stats_tableDuration_dayMins_BMI = Summary_stats_table(data = data, Construct = "BMI")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_BMI)



Summary_stats_tableDuration_dayMins_QoL = Summary_stats_table(data = data, Construct = "QoL")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_QoL)


Summary_stats_tableDuration_dayMins_HFrEF_Yes  = Summary_stats_table(data = data, Construct = "HFrEF_Yes2")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_HFrEF_Yes)


Summary_stats_tableDuration_dayMins_SelfEfficacy  = Summary_stats_table(data = data, Construct = "SelfEfficacy")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_SelfEfficacy)



Summary_stats_tableDuration_dayMins_Employment  = Summary_stats_table(data = data, Construct = "Employment2")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Employment)


Summary_stats_tableDuration_dayMins_NegativeAttitude2  = Summary_stats_table(data = data, Construct = "NegativeAttitude")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_NegativeAttitude2)


Summary_stats_tableDuration_dayMins_PositiveAttitude2  = Summary_stats_table(data = data, Construct = "PositiveAttitude")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_PositiveAttitude2)



Summary_stats_tableDuration_dayMins_Symptoms  = Summary_stats_table(data = data, Construct = "Symptoms")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Symptoms)







write.table(Results_Duration_dayMins, file = paste(OUTPUT_ROOT, "Results_Duration_dayMins.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_stats_tableResults_Duration_dayMins, file = paste(OUTPUT_ROOT, "Summary_stats_tableResults_Duration_dayMins.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )


########
Summary_stats_tableResults_Duration_dayMins$QualplusQuantSD = sqrt(Summary_stats_tableResults_Duration_dayMins$variance_quant)


Summary_stats_tableResults_Duration_dayMins = Summary_stats_tableResults_Duration_dayMins %>% 
  mutate_if(is.numeric, round, digits = 2)



Summary_stats_tableResults_Duration_dayMins$Likelihood_CI = paste("[", Summary_stats_tableResults_Duration_dayMins$Likelihood_qual_quantile_0.05, ";", Summary_stats_tableResults_Duration_dayMins$Likelihood_qual_quantile_0.95, "]", sep = "")
Summary_stats_tableResults_Duration_dayMins$Likelihood_qual_quantile_0.05 = Summary_stats_tableResults_Duration_dayMins$Likelihood_CI


Summary_stats_tableResults_Duration_dayMins = data.frame(Summary_stats_tableResults_Duration_dayMins$Construct,
                                                  
                                                  Summary_stats_tableResults_Duration_dayMins$Likelihood_qual_quantile_0.50,
                                                  Summary_stats_tableResults_Duration_dayMins$Likelihood_qual_quantile_0.05,
                                                  
                                                  Summary_stats_tableResults_Duration_dayMins$QualplusQuantSD) 

colnames(Summary_stats_tableResults_Duration_dayMins) = c("Construct",
                                                   
                                                   "Expected value (log OR)", 
                                                   "95% CrI", 
                                                   "SD")



folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}

write.table(Summary_stats_tableResults_Duration_dayMins, file = paste(folder, "_edited_Summary_stats_tableResults_Duration_dayMins_QUANT.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
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

data = Results_Duration_dayMins




Age_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Age")
SixMWT_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "6MWT")

Comorbidity_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Comorbidity")


Ethnicity_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Ethnicity")

PhysicalFunctioning7_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PhysicalFunctioning")
BMI_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "BMI")
QoL_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "QoL")

HFrEF_Yes_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "HFrEF_Yes2")
SelfEfficacy_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "SelfEfficacy")
Employment_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Employment2")
NegativeAttitude2_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "NegativeAttitude")
PositiveAttitude2_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PositiveAttitude")
Symptoms_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Symptoms")
Depression_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Depression")




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
           rep(14, 1000))

length(height)
density_ALL_Construct_quant_stratified = rbind(Age_density_by_Construct_stratified,
                                             
                                               SixMWT_density_by_Construct_stratified, 
                                               Comorbidity_density_by_Construct_stratified,
                                               Depression_density_by_Construct_stratified,
                                             
                                               Ethnicity_density_by_Construct_stratified, 
                                               PhysicalFunctioning7_density_by_Construct_stratified, 
                                               
                                               BMI_density_by_Construct_stratified, 
                                               HFrEF_Yes_density_by_Construct_stratified, 
                                               SelfEfficacy_density_by_Construct_stratified, 
                                               Employment_density_by_Construct_stratified, 
                                               NegativeAttitude2_density_by_Construct_stratified, 
                                               PositiveAttitude2_density_by_Construct_stratified, 
                                               Symptoms_density_by_Construct_stratified,
                                               QoL_density_by_Construct_stratified)

density_ALL_Construct_quant_stratified = cbind(density_ALL_Construct_quant_stratified, height)



#plot4 = ggplot(density_ALL_Construct_quant_stratified, aes(x = Theta, y = construct, height=posterior, group = construct)) +
#  geom_density_ridges(stat = "identity", scale = 10) +
#  xlim(0, 0.5)


#plotting likelihood (quantitative evidence only)
Plot_Likelihood_stratified = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3, 3  ) +
  
  # in the meta-analysis not stratified by the PA type, when a study included multiple PA outcomes we chose accelerometer units above energy expenditure and energy expenditure above self-report, 
  #self-report above a binary data, 
  #so when a study had multiple PA outcomes we tagged data as follows: the primary outcome for self-efficacy as that and the secondary outcome -- self-efficacy2, hense the names
  
  scale_y_discrete(labels=c("6MWT"  =     "6MWT" ,
                            "Age"       =      "Age",
                            "BMI"    =  "BMI",
                            "Comorbidity"   =  "Comorbidity",
                            "Depression"    =  "Depression",
                            "Employment2"   =        "Employment",
                            "Ethnicity"   =   "Ethnicity", 
                            
                            "PhysicalFunctioning" = "Physical Functioning", 
                            "HFrEF_Yes2"     =   "HFrEF",    
                             
                            "QoL" = "QoL", 
                            "NegativeAttitude" = "Negative Attitude", 
                            "PositiveAttitude"  = "Positive Attitude", 
                            "SelfEfficacy"    =     "Self-efficacy", 
                            "Symptoms" =   "Symptoms"))   


print(Plot_Likelihood_stratified)




folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}




ggsave(file = paste(folder, "/Plot_Likelihood_stratified_duration.pdf",  sep=""),Plot_Likelihood_stratified, width=4, height=3, units="in", scale=3)





# "AccelerometerUnits")

