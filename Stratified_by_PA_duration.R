

# Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"



source(paste(SOURCE_ROOT, "BayesUpdate_Quant.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence


source(paste(SOURCE_ROOT, "Summary_stats_table.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence and outputs the summary stats only as opposed to the entire distribution 

## THE BAYES UPDA

x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)

Duration_dayMins_data = subset(data, data$PA_Varme == "Duration_dayMins")
PA_Varme = "Duration_dayMins"




unique(Duration_dayMins_data$Construct)
Results_Duration_dayMins = data.frame()
unique(Duration_dayMins_data$PA_Varme)




Duration_dayMins_Age = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "Age")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Age)



Duration_dayMins_Ethnicity = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "Ethnicity")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Ethnicity)




Duration_dayMins_PhysicalFunctioning7 = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "PhysicalFunctioning")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_PhysicalFunctioning7)



Duration_dayMins_BMI = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "BMI")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_BMI)


Duration_dayMins_HFrEF_Yes = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "HFrEF_Yes2")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_HFrEF_Yes)



Duration_dayMins_SelfEfficacy = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "SelfEfficacy")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_SelfEfficacy)


Duration_dayMins_Employment = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "Employment2")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Employment)



Duration_dayMins_NegativeAttitude2 = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "NegativeAttitude")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_NegativeAttitude2)



Duration_dayMins_PositiveAttitude2 = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "PositiveAttitude")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_PositiveAttitude2)


Duration_dayMins_Symptoms = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "Symptoms")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Symptoms)




Duration_dayMins_6MWT = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "6MWT")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_6MWT)

Duration_dayMins_Comorbidity = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "Comorbidity")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Comorbidity)


Duration_dayMins_LVEF = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "LVEF5")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_LVEF)

Duration_dayMins_Depression = BayesUpdate_Quant(data = Duration_dayMins_data, Construct = "Depression")
Results_Duration_dayMins = rbind(Results_Duration_dayMins, Duration_dayMins_Depression)
#####






Summary_stats_tableResults_Duration_dayMins = data.frame()





Summary_stats_tableDuration_dayMins_Age = Summary_stats_table(data = Duration_dayMins_data, Construct = "Age")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Age)


Summary_stats_tableDuration_dayMins_Comorbidity = Summary_stats_table(data = Duration_dayMins_data, Construct = "Comorbidity")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Comorbidity)

Summary_stats_tableDuration_dayMins_6MWT = Summary_stats_table(data = Duration_dayMins_data, Construct = "6MWT")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_6MWT)

Summary_stats_tableDuration_dayMins_LVEF = Summary_stats_table(data = Duration_dayMins_data, Construct = "LVEF5")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_LVEF)


Summary_stats_tableDuration_dayMins_Depression = Summary_stats_table(data = Duration_dayMins_data, Construct = "Depression")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Depression)




Summary_stats_tableDuration_dayMins_Ethnicity  = Summary_stats_table(data = Duration_dayMins_data, Construct = "Ethnicity")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Ethnicity)


Summary_stats_tableDuration_dayMins_PhysicalFunctioning7  = Summary_stats_table(data = Duration_dayMins_data, Construct = "PhysicalFunctioning")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_PhysicalFunctioning7)


Summary_stats_tableDuration_dayMins_BMI = Summary_stats_table(data = Duration_dayMins_data, Construct = "BMI")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_BMI)



Summary_stats_tableDuration_dayMins_HFrEF_Yes  = Summary_stats_table(data = Duration_dayMins_data, Construct = "HFrEF_Yes2")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_HFrEF_Yes)


Summary_stats_tableDuration_dayMins_SelfEfficacy  = Summary_stats_table(data = Duration_dayMins_data, Construct = "SelfEfficacy")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_SelfEfficacy)



Summary_stats_tableDuration_dayMins_Employment  = Summary_stats_table(data = Duration_dayMins_data, Construct = "Employment2")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Employment)


Summary_stats_tableDuration_dayMins_NegativeAttitude2  = Summary_stats_table(data = Duration_dayMins_data, Construct = "NegativeAttitude")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_NegativeAttitude2)


Summary_stats_tableDuration_dayMins_PositiveAttitude2  = Summary_stats_table(data = Duration_dayMins_data, Construct = "PositiveAttitude")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_PositiveAttitude2)



Summary_stats_tableDuration_dayMins_Symptoms  = Summary_stats_table(data = Duration_dayMins_data, Construct = "Symptoms")
Summary_stats_tableResults_Duration_dayMins = rbind(Summary_stats_tableResults_Duration_dayMins, Summary_stats_tableDuration_dayMins_Symptoms)







write.table(Results_Duration_dayMins, file = paste(OUTPUT_ROOT, "Results_Duration_dayMins.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_stats_tableResults_Duration_dayMins, file = paste(OUTPUT_ROOT, "Summary_stats_tableResults_Duration_dayMins.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



density_by_Construct_stratified = function(data, Construct){
  index = Duration_dayMins_data$Construct == Construct
  logOddsRatio = seq( -3, 4 , length=1000)
  filtered_data = filter(data, Construct == Duration_dayMins_data[index,]$Construct)
  
  
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

LVEF_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "LVEF5")
Depression_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Depression")




Ethnicity_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Ethnicity")

PhysicalFunctioning7_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PhysicalFunctioning")
BMI_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "BMI")

HFrEF_Yes_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "HFrEF_Yes2")
SelfEfficacy_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "SelfEfficacy")
Employment_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Employment2")
NegativeAttitude2_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "NegativeAttitude")
PositiveAttitude2_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PositiveAttitude")
Symptoms_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Symptoms")




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
                                               LVEF_density_by_Construct_stratified, 
                                               Depression_density_by_Construct_stratified,
                                             
                                               Ethnicity_density_by_Construct_stratified, 
                                               PhysicalFunctioning7_density_by_Construct_stratified, 
                                               
                                               BMI_density_by_Construct_stratified, 
                                               HFrEF_Yes_density_by_Construct_stratified, 
                                               SelfEfficacy_density_by_Construct_stratified, 
                                               Employment_density_by_Construct_stratified, 
                                               NegativeAttitude2_density_by_Construct_stratified, 
                                               PositiveAttitude2_density_by_Construct_stratified, 
                                               Symptoms_density_by_Construct_stratified)

density_ALL_Construct_quant_stratified = cbind(density_ALL_Construct_quant_stratified, height)



#plot4 = ggplot(density_ALL_Construct_quant_stratified, aes(x = Theta, y = construct, height=posterior, group = construct)) +
#  geom_density_ridges(stat = "identity", scale = 10) +
#  xlim(0, 0.5)


#plotting likelihood (quantitative evidence only)
Plot_Likelihood_stratified = ggplot(density_ALL_Construct_quant_stratified, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-3, 4  ) +
  
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
                            
                            "PhysicalFunctioning" = "PhysicalFunctioning", 
                            "HFrEF_Yes2"     =   "HFrEF_Yes",    
                            "LVEF5"    =         "LVEF",    
                            
                            "NegativeAttitude" = "NegativeAttitude", 
                            "PositiveAttitude"  = "PositiveAttitude", 
                            "SelfEfficacy"    =     "SelfEfficacy", 
                            "Symptoms" =   "Symptoms"))   


print(Plot_Likelihood_stratified)


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)


x_directory_quant <- file.path(paste(c(OUTPUT_ROOT, PA_Varme), "/PLOTS_QUANT_stratified_by_PA", sep=""))

dir.create(x_directory_quant)
file.copy(from=plots.png.paths, to=x_directory_quant)





# "AccelerometerUnits")

