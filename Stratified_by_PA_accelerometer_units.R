

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

AccelerometerUnits_data = subset(data, data$PA_Varme == "AccelerometerUnits")
PA_Varme = "AccelerometerUnits"




unique(AccelerometerUnits_data$Construct)
Results_AccelerometerUnits = data.frame()
unique(AccelerometerUnits_data$PA_Varme)




AccelerometerUnits_Age = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "Age")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_Age)



AccelerometerUnits_Ethnicity = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "Ethnicity")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_Ethnicity)




AccelerometerUnits_PhysicalFunctioning7 = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "PhysicalFunctioning2")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_PhysicalFunctioning7)



AccelerometerUnits_BMI = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "BMI")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_BMI)



AccelerometerUnits_PositiveAttitude2 = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "PositiveAttitude")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_PositiveAttitude2)




AccelerometerUnits_6MWT = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "6MWT")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_6MWT)



AccelerometerUnits_Depression = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "Depression2")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_Depression)
#####


AccelerometerUnits_RenalFunction = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "RenalFunction")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_RenalFunction)


AccelerometerUnits_highproBNP = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "highproBNP")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_highproBNP)


AccelerometerUnits_LAV = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "LAV")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_LAV)


AccelerometerUnits_LVR = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "LVR")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_LVR)


AccelerometerUnits_Digoxin = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "Digoxin")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_Digoxin)


AccelerometerUnits_Doppler = BayesUpdate_Quant(data = AccelerometerUnits_data, Construct = "Doppler")
Results_AccelerometerUnits = rbind(Results_AccelerometerUnits, AccelerometerUnits_Doppler)





Summary_stats_tableResults_AccelerometerUnits = data.frame()





Summary_stats_tableAccelerometerUnits_Age = Summary_stats_table(data = AccelerometerUnits_data, Construct = "Age")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_Age)



Summary_stats_tableAccelerometerUnits_6MWT = Summary_stats_table(data = AccelerometerUnits_data, Construct = "6MWT")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_6MWT)



Summary_stats_tableAccelerometerUnits_Depression = Summary_stats_table(data = AccelerometerUnits_data, Construct = "Depression2")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_Depression)


Summary_stats_tableAccelerometerUnits_PhysicalFunctioning7  = Summary_stats_table(data = AccelerometerUnits_data, Construct = "PhysicalFunctioning2")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_PhysicalFunctioning7)


Summary_stats_tableAccelerometerUnits_Ethnicity  = Summary_stats_table(data = AccelerometerUnits_data, Construct = "Ethnicity")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_Ethnicity)




Summary_stats_tableAccelerometerUnits_BMI = Summary_stats_table(data = AccelerometerUnits_data, Construct = "BMI")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_BMI)





Summary_stats_tableAccelerometerUnits_PositiveAttitude2  = Summary_stats_table(data = AccelerometerUnits_data, Construct = "PositiveAttitude")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_PositiveAttitude2)


Summary_stats_tableAccelerometerUnits_RenalFunction  = Summary_stats_table(data = AccelerometerUnits_data, Construct = "RenalFunction")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_RenalFunction)



Summary_stats_tableAccelerometerUnits_highproBNP  = Summary_stats_table(data = AccelerometerUnits_data, Construct = "highproBNP")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_highproBNP)


Summary_stats_tableAccelerometerUnits_LAV  = Summary_stats_table(data = AccelerometerUnits_data, Construct = "LAV")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_LAV)


Summary_stats_tableAccelerometerUnits_LVR  = Summary_stats_table(data = AccelerometerUnits_data, Construct = "LVR")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_LVR)


Summary_stats_tableAccelerometerUnits_Digoxin  = Summary_stats_table(data = AccelerometerUnits_data, Construct = "Digoxin")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_Digoxin)


Summary_stats_tableAccelerometerUnits_Doppler  = Summary_stats_table(data = AccelerometerUnits_data, Construct = "Doppler")
Summary_stats_tableResults_AccelerometerUnits = rbind(Summary_stats_tableResults_AccelerometerUnits, Summary_stats_tableAccelerometerUnits_Doppler)





write.table(Results_AccelerometerUnits, file = paste(OUTPUT_ROOT, "Results_AccelerometerUnits.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_stats_tableResults_AccelerometerUnits, file = paste(OUTPUT_ROOT, "Summary_stats_tableResults_AccelerometerUnits.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



density_by_Construct_stratified = function(data, Construct){
  index = AccelerometerUnits_data$Construct == Construct
  logOddsRatio = seq( -3, 4 , length=1000)
  filtered_data = filter(data, Construct == AccelerometerUnits_data[index,]$Construct)
  
  
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

data = Results_AccelerometerUnits




Age_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Age")
SixMWT_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "6MWT")

Depression_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Depression2")

Ethnicity_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Ethnicity")

PhysicalFunctioning7_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PhysicalFunctioning2")
BMI_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "BMI")

PositiveAttitude2_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "PositiveAttitude")

RenalFunction_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "RenalFunction")
highproBNP_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "highproBNP")

LAV_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "LAV")
LVR_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "LVR")
Digoxin_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Digoxin")
Doppler_density_by_Construct_stratified = density_by_Construct_stratified(data = data, Construct = "Doppler")






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
           rep(13, 1000))

length(height)
density_ALL_Construct_quant_stratified = rbind(Age_density_by_Construct_stratified,
                                               
                                               SixMWT_density_by_Construct_stratified, 
                                               Depression_density_by_Construct_stratified,
                                               
                                               Ethnicity_density_by_Construct_stratified, 
                                               PhysicalFunctioning7_density_by_Construct_stratified, 
                                               
                                               BMI_density_by_Construct_stratified, 
                                               PositiveAttitude2_density_by_Construct_stratified, 
                                               RenalFunction_density_by_Construct_stratified, 
                                               highproBNP_density_by_Construct_stratified, 
                                               LAV_density_by_Construct_stratified, 
                                               LVR_density_by_Construct_stratified, 
                                               Digoxin_density_by_Construct_stratified, 
                                               Doppler_density_by_Construct_stratified)

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
                            "Depression2"    =  "Depression",
                            "Ethnicity"   =   "Ethnicity", 
                            
                            "PhysicalFunctioning2" = "PhysicalFunctioning", 
                     
                            
                          
                            "PositiveAttitude"  = "PositiveAttitude", 
                            
                           "RenalFunction" = "RenalFunction", 
                           "highproBNP" =  "highproBNP", 
                            
                           "LAV" =   "LAV", 
                           "LVR" = "LVR", 
                           "Digoxin" =    "Digoxin", 
                           "Doppler" ="Doppler"))


print(Plot_Likelihood_stratified)


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)


x_directory_quant <- file.path(paste(c(OUTPUT_ROOT, PA_Varme), "/PLOTS_QUANT_stratified_by_PA", sep=""))

dir.create(x_directory_quant)
file.copy(from=plots.png.paths, to=x_directory_quant)




