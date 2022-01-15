

# this script sources Baesian meta-analysis that includes quantitative evidence only from 10 different seeds 

Source_seed = function(uncertainty, seed) {
  
  source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/BayesUpdateJaarsmaQuant.R', local = TRUE)
  

  MAP_uncertainty_seed_tag = data.frame(Pooled_LOGOdds_Ratio_posterior_string_SEED,
                                        LowerCI_LogOddsRatio_posterior_string_SEED, 
                                        UpperCI_LogOddsRatio_posterior_string_SEED,
                                        posterior_alpha_Ratio_posterior_string_SEED,
                                        posterior_beta_Ratio_posterior_string_SEED, 
                                        mean_posterior_string_SEED, 
                                        posterior_CredibleInterval_0.05_string_SEED, 
                                        posterior_CredibleInterval_0.95_string_SEED, 
                                        uncertainty, 
                                        seed)

  names(MAP_uncertainty_seed_tag) <- c("LogOR", 
                                       "LogOR_CI_lower",
                                       "LogOR_CI_upper",
                                       "posterior_alpha", 
                                       "posterior_beta",
                                       "MAP", 
                                       "posterior_CredibleInterval_0.05", 
                                       "posterior_CredibleInterval_0.95", 
                                       "uncertainty", 
                                       "seed")
  print("after naming")
  
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
  
  MAP_uncertainty_seed_tag = cbind(MAP_uncertainty_seed_tag, Construct_name)
  
  return(MAP_uncertainty_seed_tag)   
  
}


uncertaintyLevels_MAP = Source_seed(uncertainty = 10, seed = 888100)


MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888101)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888102)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888103)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888104)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888105)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888106)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888107)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888108)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888110)

uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

MAP_uncertainty_seed_tag = Source_seed(uncertainty = 10, seed = 888111)
uncertaintyLevels_MAP = rbind(uncertaintyLevels_MAP, MAP_uncertainty_seed_tag)

#safe files: 

file_uncertaintyLevels_MAP <- file.path("/Users/aliya/my_docs/proj/bayesian_meta_analysis/seeds_QUANT_MAP_QUANT_23Feb2021_QUANT.csv")
fn_uncertaintyLevels_MAP <- as.filename(file_uncertaintyLevels_MAP)
make_path(fn_uncertaintyLevels_MAP)
write.table(uncertaintyLevels_MAP, file = file_uncertaintyLevels_MAP, 
            append = FALSE, 
            quote = TRUE, 
            sep = ",", 
            eol = "\r", 
            na = "NA", 
            dec = ".",
            row.names = FALSE, 
            col.names = TRUE, 
            fileEncoding = "" )