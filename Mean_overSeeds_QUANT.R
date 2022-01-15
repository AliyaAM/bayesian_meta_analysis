

data_All_SEEDS = read.csv("/Users/aliya/my_docs/proj/bayesian_meta_analysis/seeds_MAPQuant_only.csv")
head(data_All_SEEDS)


Mean_overSeeds = function(data, Construct_name){
  
  index = data$Construct_name == Construct_name
  
  data_filtered_SEEDS = filter(data, Construct_name == data[index,]$Construct_name)
  
  LogOR_mean_uncertaintyLevels_MAP =  mean(data_filtered_SEEDS$LogOR)
  LogOR_CI_lower_mean_uncertaintyLevels_MAP=  mean(data_filtered_SEEDS$LogOR_CI_lower)
  LogOR_CI_upper_mean_uncertaintyLevels_MAP=  mean(data_filtered_SEEDS$LogOR_CI_upper)
  posterior_alpha_mean_uncertaintyLevels_MAP = mean(data_filtered_SEEDS$posterior_alpha)
  posterior_beta_mean_uncertaintyLevels_MAP= mean(data_filtered_SEEDS$posterior_beta)
  MAPmean_uncertaintyLevels_MAP = mean(data_filtered_SEEDS$MAP) 
  posterior_CredibleInterval_0.05_mean_uncertaintyLevels_MAP = mean(data_filtered_SEEDS$posterior_CredibleInterval_0.05)
  posterior_CredibleInterval_0.95_mean_uncertaintyLevels_MAP = mean(data_filtered_SEEDS$posterior_CredibleInterval_0.95)
  
  
  Mean_overSEEDS = cbind(LogOR_mean_uncertaintyLevels_MAP, LogOR_CI_lower_mean_uncertaintyLevels_MAP,LogOR_CI_upper_mean_uncertaintyLevels_MAP,  posterior_alpha_mean_uncertaintyLevels_MAP,posterior_beta_mean_uncertaintyLevels_MAP, MAPmean_uncertaintyLevels_MAP,posterior_CredibleInterval_0.05_mean_uncertaintyLevels_MAP,posterior_CredibleInterval_0.95_mean_uncertaintyLevels_MAP)
  
  return(Mean_overSEEDS)
}


Age_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Age")
Comorbidity_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Comorbidity")
SocialSupport_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "SocialSupport")
NegativeAttitute_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "NegativeAttitute")
PositiveAttitute_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "PositiveAttitute")
SixMWT_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "6MWT")
Functioning_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Functioning")
Symptoms_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Symptoms")
LVEF_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "LVEF")
SelfEfficacy_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "SelfEfficacy")
Depression_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Depression")
Digoxin_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Digoxin")
Doppler_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Doppler")
Dysphoria_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Dysphoria")
Education_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Education")
Employment_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Employment")
Ethnicity_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Ethnicity")
Functioning_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Functioning")
HFDuration_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "HFDuration")
HFrEF_Yes_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "HFrEF_Yes")
highproBNP_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "highproBNP")
Hostility_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Hostility")
Income_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Income")
LAV_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "LAV")
LVAD_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "LVAD")
LVR_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "LVR")
Partner_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Partner")
PeakVO2_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "PeakVO2")
PercievedExersion_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "PercievedExersion")
QoL_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "QoL")
RenalFunction_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "RenalFunction")
Smoking_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Smoking")
Symptoms_distress_Mean_overSEEDS = Mean_overSeeds(data = data_All_SEEDS, Construct_name = "Symptoms_distress")



Mean_results_over_SEEDS = rbind(Age_Mean_overSEEDS,
                                Comorbidity_Mean_overSEEDS,
                                SocialSupport_Mean_overSEEDS,
                                NegativeAttitute_Mean_overSEEDS,
                                PositiveAttitute_Mean_overSEEDS,
                                SixMWT_Mean_overSEEDS, 
                                Functioning_Mean_overSEEDS,
                                Symptoms_Mean_overSEEDS,
                                LVEF_Mean_overSEEDS, 
                                SelfEfficacy_Mean_overSEEDS,
                                Depression_Mean_overSEEDS,
                                Digoxin_Mean_overSEEDS,
                                Doppler_Mean_overSEEDS,
                                Dysphoria_Mean_overSEEDS,
                                Education_Mean_overSEEDS,
                                Employment_Mean_overSEEDS,
                                Ethnicity_Mean_overSEEDS,
                                Functioning_Mean_overSEEDS,
                                HFDuration_Mean_overSEEDS,
                                HFrEF_Yes_Mean_overSEEDS,
                                highproBNP_Mean_overSEEDS,
                                Hostility_Mean_overSEEDS,
                                Income_Mean_overSEEDS,
                                LAV_Mean_overSEEDS,
                                LVAD_Mean_overSEEDS,
                                LVR_Mean_overSEEDS,
                                Partner_Mean_overSEEDS,
                                PeakVO2_Mean_overSEEDS,
                                PercievedExersion_Mean_overSEEDS,
                                QoL_Mean_overSEEDS,
                                RenalFunction_Mean_overSEEDS,
                                Smoking_Mean_overSEEDS,
                                Symptoms_distress_Mean_overSEEDS)

Construct_name = c("Age",
                   "Comorbidity",
                   "SocialSupport",
                   "NegativeAttitute",
                   "PositiveAttitute",
                   "6MWT",
                   "Functioning",
                   "Symptoms",
                   "LVEF",
                   "SelfEfficacy",
                   "Depression",
                   "Digoxin",
                   "Doppler",
                   "Dysphoria",
                   "Education", 
                   "Employment",
                   "Ethnicity",
                   "Functioning",
                   "HFDuration",
                   "HFrEF_Yes",
                   "highproBNP",
                   "Hostility",
                   "Income",
                   "LAV",
                   "LVAD",
                   "LVR",
                   "Partner",
                   "PeakVO2",
                   "PercievedExersion",
                   "QoL",
                   "RenalFunction",
                   "Smoking",
                   "Symptoms_distress")

Mean_results_over_SEEDS = cbind(Mean_results_over_SEEDS, Construct_name)

file_Mean_results_over_SEEDS <- file.path("/Users/aliya/my_docs/proj/bayesian_meta_analysis/Quant_Mean_results_over_SEEDS.csv")
fn_Mean_results_over_SEEDS <- as.filename(file_Mean_results_over_SEEDS)
make_path(fn_Mean_results_over_SEEDS)
write.table(Mean_results_over_SEEDS, file = file_Mean_results_over_SEEDS, 
            append = FALSE, 
            quote = TRUE, 
            sep = ",", 
            eol = "\r", 
            na = "NA", 
            dec = ".",
            row.names = FALSE, 
            col.names = TRUE, 
            fileEncoding = "" )