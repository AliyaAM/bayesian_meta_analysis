

# Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"



source(paste(SOURCE_ROOT, "BayesUpdate_Quant.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence


source(paste(SOURCE_ROOT, "Summary_stats_table.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence and outputs the summary stats only as opposed to the entire distribution 

## THE BAYES UPDA

x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
All_extracted_data  = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)

data = subset(All_extracted_data, All_extracted_data$PA_Varme == "Steps/d_total")
PA_Varme = "Steps_day"
#PhysicalFunctioning
#6MWT
#Age
#LVEF
#PeakVO2
#proBNP
#Depression
#SelfEfficacy

unique(data$Construct)
Results_Steps_day = data.frame()
unique(data$PA_Varme)




source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep="")) #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 
likelihood_data =  ConvertEffectsizes(data = data)


Steps_day_6MWT = BayesUpdate_Quant(data = data, Construct = "6MWT")
Results_Steps_day = rbind(Results_Steps_day, Steps_day_6MWT)
Steps_day_LVEF = BayesUpdate_Quant(data = data, Construct = "LVEF")
Results_Steps_day = rbind(Results_Steps_day, Steps_day_LVEF)
Steps_day_PeakVO2 = BayesUpdate_Quant(data = data, Construct = "PeakVO2")
Results_Steps_day = rbind(Results_Steps_day, Steps_day_PeakVO2)
Steps_day_proBNP = BayesUpdate_Quant(data = data, Construct = "proBNP")
Results_Steps_day = rbind(Results_Steps_day, Steps_day_proBNP)


Steps_day_PhysicalFunctioning = BayesUpdate_Quant(data = data, Construct = "PhysicalFunctioning")
Results_Steps_day = rbind(Results_Steps_day, Steps_day_PhysicalFunctioning)



Summary_stats_tableResults_Steps_day = data.frame()

Summary_stats_tableSteps_day_6MWT = Summary_stats_table(data = data, Construct = "6MWT")
Summary_stats_tableResults_Steps_day = rbind(Summary_stats_tableResults_Steps_day, Summary_stats_tableSteps_day_6MWT)

Summary_stats_tableSteps_day_LVEF = Summary_stats_table(data = data, Construct = "LVEF")
Summary_stats_tableResults_Steps_day = rbind(Summary_stats_tableResults_Steps_day, Summary_stats_tableSteps_day_LVEF)

Summary_stats_tableSteps_day_PeakVO2 = Summary_stats_table(data = data, Construct = "PeakVO2")
Summary_stats_tableResults_Steps_day = rbind(Summary_stats_tableResults_Steps_day, Summary_stats_tableSteps_day_PeakVO2)

Summary_stats_tableSteps_day_proBNP = Summary_stats_table(data = data, Construct = "proBNP")
Summary_stats_tableResults_Steps_day = rbind(Summary_stats_tableResults_Steps_day, Summary_stats_tableSteps_day_proBNP)




Summary_stats_tableSteps_day_PhysicalFunctioning = Summary_stats_table(data = data, Construct = "PhysicalFunctioning")
Summary_stats_tableResults_Steps_day = rbind(Summary_stats_tableResults_Steps_day, Summary_stats_tableSteps_day_PhysicalFunctioning)



write.table(Results_Steps_day, file = paste(OUTPUT_ROOT, "Results_Steps_day.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_stats_tableResults_Steps_day, file = paste(OUTPUT_ROOT, "Summary_stats_tableResults_Steps_day.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



########
Summary_stats_tableResults_Steps_day$QualplusQuantSD = sqrt(Summary_stats_tableResults_Steps_day$variance_quant)


Summary_stats_tableResults_Steps_day = Summary_stats_tableResults_Steps_day %>% 
  mutate_if(is.numeric, round, digits = 2)



Summary_stats_tableResults_Steps_day$Likelihood_CI = paste("[", Summary_stats_tableResults_Steps_day$Likelihood_qual_quantile_0.05, ";", Summary_stats_tableResults_Steps_day$Likelihood_qual_quantile_0.95, "]", sep = "")
Summary_stats_tableResults_Steps_day$Likelihood_qual_quantile_0.05 = Summary_stats_tableResults_Steps_day$Likelihood_CI


Summary_stats_tableResults_Steps_day = data.frame(Summary_stats_tableResults_Steps_day$Construct,
                                                           
                                                           Summary_stats_tableResults_Steps_day$Likelihood_qual_quantile_0.50,
                                                           Summary_stats_tableResults_Steps_day$Likelihood_qual_quantile_0.05,
                                                           
                                                           Summary_stats_tableResults_Steps_day$QualplusQuantSD) 

colnames(Summary_stats_tableResults_Steps_day) = c("Construct",
                                                            
                                                            "Expected value (log OR)", 
                                                            "95% CrI", 
                                                            "SD")

folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}

write.table(Summary_stats_tableResults_Steps_day, file = paste(folder, "_edited_Summary_stats_tableResults_Steps_day_QUANT.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )


density_by_Construct_stratified = function(data, Construct){
  index = data$Construct == Construct
  logOddsRatio = seq( -6, 9 , length=1000)
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

data = Results_Steps_day

SixMWT_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "6MWT")
PhysicalFunctioning_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PhysicalFunctioning")
LVEF_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "LVEF")
highproBNP_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "proBNP")
PeakVO2_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PeakVO2")



height = c(rep(1, 1000),
           rep(2, 1000), 
           rep(3, 1000), 
           rep(4, 1000), 
           rep(5, 1000))

length(height)
density_ALL_Construct_quant_stratified = rbind(SixMWT_density_by_Construct_stratified, 
                                               PhysicalFunctioning_density_by_Construct_stratified,
                                               LVEF_density_by_Construct_stratified, 
                                               highproBNP_density_by_Construct_stratified,
                                               PeakVO2_density_by_Construct_stratified)

density_ALL_Construct_quant_stratified = cbind(density_ALL_Construct_quant_stratified, height)



#plot4 = ggplot(density_ALL_Construct_quant_stratified, aes(x = Theta, y = construct, height=posterior, group = construct)) +
#  geom_density_ridges(stat = "identity", scale = 10) +
#  xlim(0, 0.5)


#plotting likelihood (quantitative evidence only)
Plot_Likelihood_stratified = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-6, 9)+
  scale_y_discrete(labels=c("6MWT"  =     "6MWT" ,
                            "PhysicalFunctioning" = "Physical Functioning", 
                            "LVEF"    =         "LVEF",    
                            "proBNP" =  "proBNP", 
                            "PeakVO2" = "Peak VO2"))   + 

  theme(text = element_text(size = 25))   



print(Plot_Likelihood_stratified)


folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}
ggsave(file = paste(folder, "/Plot_Likelihood_stratified_steps_per_day.pdf",  sep=""),Plot_Likelihood_stratified, width=4, height=3, units="in", scale=3)



#IPAQ_scale = PA_type_stratified(data = data, PA_Varme = "IPAQ_scale")

#Exercise_complient_Binary = PA_type_stratified(data = data, PA_Varme = "Exercise_complient_Binary")


#EnergyExpend_total = PA_type_stratified(data = data, PA_Varme = "EnergyExpend_total")


#Duration_dayMins = PA_type_stratified(data = data, PA_Varme = "Duration_dayMins")


#Duration_dayMins = PA_type_stratified(data = data, PA_Varme = "AccelerometerUnits")


