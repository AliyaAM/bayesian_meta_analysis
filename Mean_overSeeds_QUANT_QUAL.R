

#read the results from the analysis of the qualitative combined with quantitative evidence for all 10 different seeds: 



data_All_SEEDS = read.csv(paste(SOURCE_ROOT, "seeds_MAPQualQuant.csv", sep=""))

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

Mean_results_over_SEEDS = rbind(Age_Mean_overSEEDS,
                                Comorbidity_Mean_overSEEDS,
                                SocialSupport_Mean_overSEEDS,
                                NegativeAttitute_Mean_overSEEDS,
                                PositiveAttitute_Mean_overSEEDS,
                                SixMWT_Mean_overSEEDS, 
                                Functioning_Mean_overSEEDS,
                                Symptoms_Mean_overSEEDS,
                                LVEF_Mean_overSEEDS, 
                                SelfEfficacy_Mean_overSEEDS)

Construct_name = c("Age",
                   "Comorbidity",
                   "SocialSupport",
                   "NegativeAttitute",
                   "PositiveAttitute",
                   "6MWT",
                   "Functioning",
                   "Symptoms",
                   "LVEF",
                   "SelfEfficacy")

Mean_results_over_SEEDS = cbind(Mean_results_over_SEEDS, Construct_name)


file_Mean_results_over_SEEDS <- file.path(paste(OUTPUT_ROOT, "Mean_results_over_SEEDS_Qual_and_Quant_FINAL_RESULT.csv", sep=""))
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