

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

data = subset(All_extracted_data, All_extracted_data$PA_Varme == "IPAQ_scale")
PA_Varme = "IPAQ_scale"
#PhysicalFunctioning
#6MWT
#Age
#LVEF
#PeakVO2
#proBNP
#Depression
#SelfEfficacy

unique(data$Construct)
Results_IPAQ_scale = data.frame()



source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep="")) #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 
likelihood_data =  ConvertEffectsizes(data = data)



IPAQ_scale_PerceivedExertion = BayesUpdate_Quant(data = data, Construct = "PerceivedExertion")
Results_IPAQ_scale = rbind(Results_IPAQ_scale, IPAQ_scale_PerceivedExertion)

IPAQ_scale_SelfEfficacy = BayesUpdate_Quant(data = data, Construct = "SelfEfficacy")
Results_IPAQ_scale = rbind(Results_IPAQ_scale, IPAQ_scale_SelfEfficacy)




Summary_stats_tableResults_IPAQ_scale = data.frame()
Summary_stats_tableIPAQ_scale_PerceivedExertion = Summary_stats_table(data = data, Construct = "PerceivedExertion")
Summary_stats_tableResults_IPAQ_scale = rbind(Summary_stats_tableResults_IPAQ_scale, Summary_stats_tableIPAQ_scale_PerceivedExertion)


Summary_stats_tableIPAQ_scale_SelfEfficacy = Summary_stats_table(data = data, Construct = "SelfEfficacy")
Summary_stats_tableResults_IPAQ_scale = rbind(Summary_stats_tableResults_IPAQ_scale, Summary_stats_tableIPAQ_scale_SelfEfficacy)



write.table(Results_IPAQ_scale, file = paste(OUTPUT_ROOT, "Results_IPAQ_scale.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_stats_tableResults_IPAQ_scale, file = paste(OUTPUT_ROOT, "Summary_stats_tableResults_IPAQ_scale.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



########
Summary_stats_tableResults_IPAQ_scale$QualplusQuantSD = sqrt(Summary_stats_tableResults_IPAQ_scale$variance_quant)


Summary_stats_tableResults_IPAQ_scale = Summary_stats_tableResults_IPAQ_scale %>% 
  mutate_if(is.numeric, round, digits = 2)



Summary_stats_tableResults_IPAQ_scale$Likelihood_CI = paste("[", Summary_stats_tableResults_IPAQ_scale$Likelihood_qual_quantile_0.05, ";", Summary_stats_tableResults_IPAQ_scale$Likelihood_qual_quantile_0.95, "]", sep = "")
Summary_stats_tableResults_IPAQ_scale$Likelihood_qual_quantile_0.05 = Summary_stats_tableResults_IPAQ_scale$Likelihood_CI


Summary_stats_tableResults_IPAQ_scale = data.frame(Summary_stats_tableResults_IPAQ_scale$Construct,
                                                                  
                                                                  Summary_stats_tableResults_IPAQ_scale$Likelihood_qual_quantile_0.50,
                                                                  Summary_stats_tableResults_IPAQ_scale$Likelihood_qual_quantile_0.05,
                                                                  
                                                                  Summary_stats_tableResults_IPAQ_scale$QualplusQuantSD) 

colnames(Summary_stats_tableResults_IPAQ_scale) = c("Construct",
                                                                   
                                                                   "Expected value (log OR)", 
                                                                   "95% CrI", 
                                                                   "SD")


folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}



write.table(Summary_stats_tableResults_IPAQ_scale, file = paste(folder, "_edited_Summary_stats_tableResults_IPAQ_scale_QUANT.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
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

data = Results_IPAQ_scale

PerceivedExertion_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PerceivedExertion")
SelfEfficacy_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "SelfEfficacy")



height = c(rep(1, 1000),
           rep(2, 1000))

length(height)
density_ALL_Construct_quant_stratified = rbind(PerceivedExertion_density_by_Construct_stratified, 
                                             
                                               SelfEfficacy_density_by_Construct_stratified)

density_ALL_Construct_quant_stratified = cbind(density_ALL_Construct_quant_stratified, height)



#plot4 = ggplot(density_ALL_Construct_quant_stratified, aes(x = Theta, y = construct, height=posterior, group = construct)) +
#  geom_density_ridges(stat = "identity", scale = 10) +
#  xlim(0, 0.5)


#plotting likelihood (quantitative evidence only)
Plot_Likelihood_stratified_IPAQ = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3, 3)+
  scale_y_discrete(labels=c("PerceivedExertion"  =     "Perceived Exertion" ,
                            "SelfEfficacy" = "Self-efficacy"))   + 
  
  theme(text = element_text(size = 25))   
print(Plot_Likelihood_stratified_IPAQ)



folder = paste(OUTPUT_ROOT, "stratified_by_PA_results/",  sep="")
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}


ggsave(file = paste(folder, "/Plot_Likelihood_stratified_IPAQ.pdf",  sep=""),Plot_Likelihood_stratified_IPAQ, width=4, height=3, units="in", scale=3)




#IPAQ_scale = PA_type_stratified(data = data, PA_Varme = "IPAQ_scale")

#Exercise_complient_Binary = PA_type_stratified(data = data, PA_Varme = "Exercise_complient_Binary")


#EnergyExpend_total = PA_type_stratified(data = data, PA_Varme = "EnergyExpend_total")


#Duration_dayMins = PA_type_stratified(data = data, PA_Varme = "Duration_dayMins")


#Duration_dayMins = PA_type_stratified(data = data, PA_Varme = "AccelerometerUnits")


